-module(nodo).
-export([init/0, read_from_shared_folder/0, pprint/1, download_file/2, register/0, shell/0, comm_handler/1]).
-include("config.hrl").
-include("gen_header_tcp.hrl").
% USAGE:
% c(server).
% server:server().

% nc localhost 12345 (o alternativamente compilar nodo y descargar desde la cli)

% Genera un nombre aleatorio de tamaño Sz, con mayusculas, minusculas y digitos
name_generator(Sz) ->
    Alphabet = lists:append([lists:seq(65,90), lists:seq(48,57), lists:seq(97,122)]),
    N = length(Alphabet),
    lists:map(fun(_) -> lists:nth(rand:uniform(N), Alphabet)
        end,
        lists:seq(1, Sz)
    ).

% Imprime de manera indexada y tabulada los elementos de una lista
pprint(List) ->
    IndList = lists:zip(lists:seq(1, length(List)), List),
    lists:foreach(fun({Ind, X}) -> io:format("~p. ~p ~n", [Ind, X]) end, IndList).

% Crea las carpetas del nodo si no están creadas.
check_dirs() ->
    case filelib:is_dir(?SHARED_PATH) of
        true ->
            io:format("La carpeta compartida esta presente ~n");
        false ->
            file:make_dir("compartida"),
            io:format("Se creo la carpeta compartida~n")
    end,
    case filelib:is_dir(?DOWNLOADS_PATH) of
        true ->
            io:format("La carpeta descargas esta presente ~n");
        false ->
            file:make_dir("descargas"),
            io:format("Se creo la carpeta descargas~n")
    end,
    ok.

% Leer del directorio compartido, y retornar una lista con los archivos en él
read_from_shared_folder() ->
    case file:list_dir(?SHARED_PATH) of
        {ok, RawFileNames} ->
            % excluye por ejemplo links simbolicos, carpetas, sockets, pipes, etc
            % el join es porque is_regular toma rutas, no nombres de archivo "sueltos"
            [F || F <- RawFileNames, filelib:is_regular(filename:join(?SHARED_PATH , F))];
        {error, Reason} ->
            io:format("Error leyendo directorio ~p: ~p ~n", [?SHARED_PATH], [Reason]),
            [] 
    end.

% Metodo que registra al nodo en la red
register() ->
    io:format("Registrar al nodo en ~p ~n", [?REG_PATH]).

% Inicia la shell interactiva para el cliente
shell() ->
    io:format("linea de comandos~n"),
    shell_loop(),
    ok.

% Funcion auxiliar usada por la shell interactiva
shell_loop() ->
    io:format("~s> ", [?SHELLNAME]),
    case io:get_line("") of
        eof ->
            io:format("nos vimos~n");
        {error, ErrorDescription} ->
            io:format("Error: ~p ~n", [ErrorDescription]),
            shell_loop();
        Line ->
            % pasarle la linea, quitandole el enter
            if
                length(Line) > 1 -> comm_handler(hd(string:tokens(Line, "\n")));
                true -> ok
            end,
            shell_loop()
    end.

% Evaluar comandos
comm_handler(Input) ->
    Args = string:tokens(Input, " "),
    case Args of
        ["salir"] ->
            io:format("me las piro vampiro~n"),
            halt();
        ["id_nodo"] ->
            io:format("~p ~n", [self()]);
        ["listar_mis_archivos"] ->
            pprint(read_from_shared_folder());
        ["DOWNLOAD_REQUEST", FileName, NodeIdStr] ->
            io:format("Intentando conectarse a ~p para descargar ~s...~n", [NodeIdStr, FileName]),
            case gen_tcp:connect(NodeIdStr, ?PORT, [binary, {packet, 0}, {active, false}]) of
                {error, Reason} ->
                    io:format("Error al conectar con el nodo: ~p~n", [Reason]),
                    error;
                %% si se pudo conectar, descargar el archivo
                {ok, ConnSock} ->
                    io:format("Conectado a ~p~n", [ConnSock]),
                    case download_file(FileName, ConnSock) of
                        {error, closed} ->
                            io:format("Conexion cerrada antes de completar la descarga~n");
                        {error, not_found} ->
                            io:format("Error: archivo no encontrado en el server~n");
                        {error, empty_file} ->
                            io:format("Error: el archivo ~s esta vacio~n", [FileName]);
                        ok ->
                            gen_tcp:close(ConnSock),
                            io:format("Archivo descargado! ~n")
                    end
            end;
        ["help"] ->
            pprint(?SHELL_COMMS);
        _ ->
            io:format("Comando desconocido. 'help' para ver los comandos disponibles y su uso. ~n")
    end.

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

init() ->
    io:format("Inicializa nodo ~n"),
    check_dirs(),
    _Shr = read_from_shared_folder(),
    _Discovered = [],
    register(),
    io:format("Archivos compartidos: ~p ~n", [?TEST_NAMES]),
    
    Name = name_generator(?NODE_NAME_LENGTH),
    io:format("Nombre del nodo: ~p ~n", [Name]),

    %% TODO: registrar el nodo a traves de UDP

    % probablemente convenga dejar el server registrado haciendole spawn con otro proceso
    % server:server(),

    shell(),
    ok.
