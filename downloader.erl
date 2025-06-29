-module(downloader).
-include("config.hrl").
-export([download_file/2, read_direct/3, read_chunked/3, read_chunks_loop/3, handle_chunk/5]).

% Descarga un archivo del servidor, dado su nombre y el socket de la conexion
download_file(FileName, Sock) ->
        io:format("Descargando archivo ~s...~n", [FileName]),
        Req = "DOWNLOAD_REQUEST " ++ FileName ++ "\r\n",

        io:format("Enviando request: ~s~n", [Req]),
        ok = gen_tcp:send(Sock, Req),

        % esperar a recibir la respuesta del servidor, cuando recibo leo el primer byte
        case gen_tcp:recv(Sock, 1) of
        {error, closed} ->
            gen_tcp:close(Sock),
            {error, closed};

        {ok, <<Code:8>>} when Code =/= ?OK_CODE ->
            gen_tcp:close(Sock),
            {error, not_found};

        {ok, <<?OK_CODE:8>>} ->
            case gen_tcp:recv(Sock, + 4) of
                {error, closed} ->
                    {error, closed};
                {ok, <<FileSize:32/big-unsigned-integer>>} ->
                    case FileSize < ?MAX_SINGLE_FILE_SIZE of
                        true  ->
                            case FileSize > 0 of
                                false ->
                                    io:format("El archivo ~p no tiene contenido, no se enviara~n", [FileName]),
                                    % aca se manda notfound, aunque en realidad el error es otro
                                    gen_tcp:send(Sock, <<(?NOTFOUND_CODE):8>>),
                                    {error, empty_file};
                                true ->
                                    io:format("Archivo chico (~p B), leo de una~n", [FileSize]),
                                    read_direct(Sock, FileSize, FileName)
                            end;
                        false ->
                            case gen_tcp:recv(Sock, 4) of
                                {error, closed} ->
                                    {error, closed};
                               
                                {ok, <<ChunkSize:32>>} -> % recibir el chunk size (no se usa, se podria prefijar con '_')
                                    io:format("Archivo grande (~p B), leer de a chunks de ~p B ~n", [FileSize, ChunkSize]),
                                    read_chunked(Sock, FileSize, FileName)
                            end
                    end
            end
        end.

% Metodo para leer el archivo completo de una, sin chunks
read_direct(Sock, FileSize, Nom) ->
    case gen_tcp:recv(Sock, FileSize) of
        {error, closed} ->
            io:format("Conexion cerrada antes de recibir el archivo~n"),
            {error, closed};
        {ok, BinData} -> 
            FullPath = filename:join(?DOWNLOADS_PATH, Nom),
            ok = file:write_file(FullPath, BinData),
            io:format("Archivo guardado en: ~p~n", [FullPath]),
            ok
    end.
    
% Metodo para leer el archivo en partes, cuando el tamaño es mayor al umbral
read_chunked(Sock, FileSize, Nom) ->
    % ya lei los primeros 1 + 4 + 4 bytes, ahora lo que resta es del archivo
    % Abrir archivo
    FullPath = filename:join(?DOWNLOADS_PATH, Nom),
    case file:open(FullPath, [write, binary]) of
        {error, Reason} ->
            io:format("Error al abrir el archivo ~p: ~p~n", [FullPath, Reason]),
            {error, open_file_failed};
        {ok, File} ->
            read_chunks_loop(Sock, File, FileSize),
            file:close(File),
            gen_tcp:close(Sock),
            ok
    end.

read_chunks_loop(_Sock, _File, 0) ->
    ok;
read_chunks_loop(Sock, File, Rem) when Rem > 0 ->
    io:format("Quedan ~p bytes por recibir~n", [Rem]),

    case gen_tcp:recv(Sock, 1 + 2 + 4) of
        {error, closed} ->
            io:format("Conexion cerrada antes de recibir el chunk~n"),
            {error, closed};
        {ok, <<ChunkCode:8, _ChunkInd:16, CurrChunkSize:32/big-unsigned-integer>>} ->
            handle_chunk(Sock, File, Rem, ChunkCode, CurrChunkSize)
    end.


handle_chunk(Sock, File, Rem, ChunkCode, CurrChunkSize) ->
    case ChunkCode == ?CHUNK_CODE of
        true ->
            case gen_tcp:recv(Sock, CurrChunkSize) of
                {error, closed} ->
                    io:format("Conexion cerrada antes de recibir el chunk~n"),
                    {error, closed};
                {ok, Bin} ->
                    io:format("Recibido chunk de ~p bytes~n", [CurrChunkSize]),
                    % obs la diferencia con write_file: que no requeria que el archivo estuviera abierto 
                    % (lo hacia manualmente, pero hacer esto recursivamente es costoso)
                    ok = file:write(File, Bin),
                    read_chunks_loop(Sock, File, Rem - CurrChunkSize)
                end;
        false ->
            io:format("Error: código de chunk inválido: ~p~n", [ChunkCode]),
            {error, invalid_chunk_code}
    end.