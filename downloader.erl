-module(downloader).
-export([download_file/2, read_direct/3, read_chunked/3, read_chunks_loop/3, handle_chunk/5, send_chunks/5, send_found_file/3]).
-include("config.hrl").

% Downloads a file from the server, with the given name and socket of the conection
download_file(FileName, Sock) ->
        io:format("Descargando archivo ~s...~n", [FileName]),
        Req = "DOWNLOAD_REQUEST " ++ FileName ++ "\r\n",

        %io:format("Enviando request: ~s~n", [Req]),

        % Sending a request to the server
        ok = gen_tcp:send(Sock, Req),

        % Waiting to receive server response
        case gen_tcp:recv(Sock, 1, ?DOWLOAD_DATA_TIMEOUT) of
        {error, Reason} ->
            gen_tcp:close(Sock),
            {error, Reason};
        
        % Not the desired information, we close the socket and exit
        {ok, <<Code:8>>} when Code =/= ?OK_CODE ->
            gen_tcp:close(Sock),
            {error, not_found};

        % We receive an okay code, which means that the file has been received
        {ok, <<?OK_CODE:8>>} ->
            % quitar el "+4"
            case gen_tcp:recv(Sock, + 4, ?DOWLOAD_DATA_TIMEOUT) of
                {error, Reason} ->
                    {error, Reason};
                {ok, <<FileSize:32/big-unsigned-integer>>} ->
                    
                    case FileSize < ?MAX_SINGLE_FILE_SIZE of
                        % If we can download it in one go we do it
                        true  ->
                            case FileSize > 0 of
                                false ->
                                    io:format("El archivo ~p no tiene contenido, no se enviara~n", [FileName]),
                                    % aca se manda notfound, aunque en realidad el error es otro
                                    gen_tcp:send(Sock, <<(?NOTFOUND_CODE):8>>),
                                    {error, empty_file};
                                true ->
                                    %io:format("Archivo chico (~p B), leo de una~n", [FileSize]),
                                    read_direct(Sock, FileSize, FileName)
                            end;
                        % If not, we download it in chunks
                        false ->
                            case gen_tcp:recv(Sock, 4, ?DOWLOAD_DATA_TIMEOUT) of
                                {error, Reason} ->
                                    {error, Reason};
                               
                                {ok, <<_ChunkSize:32>>} -> % Receiving chunk size by the standart, we don't use it but we can't skip it
                                    %io:format("Archivo grande (~p B), leer de a chunks de ~p B ~n", [FileSize, ChunkSize]),
                                    read_chunked(Sock, FileSize, FileName)
                            end
                    end
            end
        end.

% Method to read a file without the use of chunks
read_direct(Sock, FileSize, Nom) ->
    case gen_tcp:recv(Sock, FileSize, ?DOWLOAD_DATA_TIMEOUT) of
        {error, Reason} ->
            io:format("Conexion cerrada antes de recibir el archivo~n"),
            {error, Reason};
        {ok, BinData} -> 
            FullPath = filename:join(?DOWNLOADS_PATH, Nom),

            % BinData holds the file data, so we write it in our downloads directory
            ok = file:write_file(FullPath, BinData),
            io:format("Archivo guardado en: ~p~n", [FullPath]),
            ok
    end.
    
% Method to download a file in multiple chunks
read_chunked(Sock, FileSize, Nom) ->
    % We already read 1 + 4 + 4 bytes, now we read the rest of the file

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

% Loop in which we analyze the multiple chunks
read_chunks_loop(_Sock, _File, 0) -> % This marks the end of the loop
    ok;
read_chunks_loop(Sock, File, Rem) when Rem > 0 ->
    %io:format("Quedan ~p bytes por recibir~n", [Rem]),

    % We receive the msg code, the chunk index and the chunk size
    case gen_tcp:recv(Sock, 1 + 2 + 4, ?DOWLOAD_DATA_TIMEOUT) of
        {error, Reason} ->
            io:format("Conexion cerrada antes de recibir el chunk~n"),
            {error, Reason};
        {ok, <<ChunkCode:8, _ChunkInd:16, CurrChunkSize:32/big-unsigned-integer>>} ->
            handle_chunk(Sock, File, Rem, ChunkCode, CurrChunkSize)
    end.

% Method to download a single chunk and write it in the specified file
handle_chunk(Sock, File, Rem, ChunkCode, CurrChunkSize) ->
    case ChunkCode == ?CHUNK_CODE of
        true ->
            case gen_tcp:recv(Sock, CurrChunkSize, ?DOWLOAD_DATA_TIMEOUT) of
                {error, Reason} ->
                    io:format("Conexion cerrada antes de recibir el chunk~n"),
                    {error, Reason};
                {ok, Bin} ->
                    %io:format("Recibido chunk de ~p bytes~n", [CurrChunkSize]),

                    ok = file:write(File, Bin),
                    % After write we return to the main loop
                    read_chunks_loop(Sock, File, Rem - CurrChunkSize)
                end;
        false ->
            io:format("Error: código de chunk inválido: ~p~n", [ChunkCode]),
            {error, invalid_chunk_code}
    end.

% Method to send the chunks of a big enough file
send_chunks(Sock, BinData, FileSize, CantChunk, N) ->
    %io:fwrite("iteracion ~p de ~p chunks~n", [N, CantChunk]),
    case N == (CantChunk - 1) of
        % Last chunk
        true -> 
            %io:fwrite("Enviando chunk ~p de ~p bytes~n", [N, ?MAX_SINGLE_FILE_SIZE]),
            ChunkSize = FileSize - (?MAX_SINGLE_FILE_SIZE * (N)),
            ChunkData = binary:part(BinData, N * (?MAX_SINGLE_FILE_SIZE), ChunkSize),
            % We always send a chunk file of ?MAX_SINGLE_FILE_SIZE except for the last one, which can have a smaller size

            % Formatting message
            Chunk = <<
                (?CHUNK_CODE):8,
                N:16,
                ChunkSize:32/big-unsigned-integer,
                ChunkData/binary
            >>,

            gen_tcp:send(Sock, Chunk),
            ok;

        false ->
            %io:fwrite("Enviando chunk ~p de ~p bytes~n", [N, ?MAX_SINGLE_FILE_SIZE]),
            ChunkData = binary:part(BinData, N * (?MAX_SINGLE_FILE_SIZE), ?MAX_SINGLE_FILE_SIZE),

            % Formatting message
            Chunk = <<
                (?CHUNK_CODE):8,
                N:16,
                (?MAX_SINGLE_FILE_SIZE):32/big-unsigned-integer,
                ChunkData/binary
            >>,

            case gen_tcp:send(Sock, Chunk) of
                ok -> 
                    %io:fwrite("Chunk enviado! ~n"),
                    ok;
                {error, Reason} ->
                    io:fwrite("Error al enviar un chunk: ~p. Envio cancelado~n", [Reason]),
                    error
            end,

            % Recursive call to continue the loop
            send_chunks(Sock, BinData, FileSize, CantChunk, N + 1)
    end.

% We send the found file to the client, if the size is lower than MAX then we do it in a single go, otherwise it uses chunks
send_found_file(Socket, FullPath, FileSize) ->

    case file:read_file(FullPath) of
        {error, Reason} ->
            io:format("Error al leer el archivo: ~p~n", [Reason]),
            gen_tcp:send(Socket, <<(?NOTFOUND_CODE):8>>),
            error;
        {ok, BinData} ->
            % We check for the size to decide the method for the shipping
            case FileSize < ?MAX_SINGLE_FILE_SIZE of
                % Single go
                true ->
                    case FileSize > 0 of
                        % The file is empty
                        false ->
                            io:format("El archivo ~p no tiene contenido, no se enviara~n", [FullPath]),
                            % Here we use not found even though it is not the error
                            gen_tcp:send(Socket, <<(?NOTFOUND_CODE):8>>),
                            error;
                        true ->

                            % Formatting message
                            Packet = <<
                                (?OK_CODE):8,
                                FileSize:32/big-unsigned-integer,
                                BinData/binary
                            >>,

                            %io:format("Paquete definido: ~p~n", [Packet]),

                            case gen_tcp:send(Socket, Packet) of
                                ok -> 
                                    io:format("Archivo enviado! ~n"),
                                    ok;
                                {error, Reason} ->
                                    io:format("Error al enviar el archivo: ~p~n", [Reason]),
                                    error
                            end,
                            io:format("Archivo enviado! ~n")
                    end;
                
                % Chunks method
                false ->
                    
                    % Formatting message
                    Packet = <<
                        (?OK_CODE):8,
                        FileSize:32/big-unsigned-integer,
                        (?MAX_SINGLE_FILE_SIZE):32/big-unsigned-integer
                    >>,

                    case gen_tcp:send(Socket, Packet) of
                        ok -> 
                            %io:format("Paquete enviado! ~n"),
                            ok;
                        {error, Reason} ->
                            io:format("Error al enviar el paquete: ~p~n", [Reason]),
                            error
                    end,

                    CantChunks = ceil(FileSize / (?MAX_SINGLE_FILE_SIZE)),
                    %io:format("Cant chunks: ~p~n", [CantChunks]),
                    send_chunks(Socket, BinData, FileSize, CantChunks, 0)
            end
    end.
