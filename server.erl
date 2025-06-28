-module(server).
-export([server/0]).
-include("config.hrl").
-include("gen_header_tcp.hrl").

% USAGE:
% c(server).
% server:server().

% nc localhost 12345 (o alternativamente compilar nodo y descargar desde la cli)

%% buscar el archivo en la carpeta compartida, si esta retornar su ruta completa y su tamaño (en bytes)
%% si no esta, retornar error
file_lookup(FileName) ->
    FullSharedPath = filename:join(?SHARED_PATH, FileName),
    FullDownPath = filename:join(?DOWNLOADS_PATH, FileName),
    case filelib:wildcard(FullSharedPath) of
        [] ->
            case filelib:wildcard(FullDownPath) of
                [] ->
                    #fileLookupError{reason = "File not found"};
                Lst ->
                    #fileLookupSuccess{files = generate_fileinfo(Lst)}
            end;
        Lst ->
            #fileLookupSuccess{files = generate_fileinfo(Lst)}
    end.

generate_fileinfo([]) -> [];
generate_fileinfo([File | FileLst]) ->
    [#fileInfo{name = File, size = filelib:file_size(File)} | generate_fileinfo(FileLst)].

%% Inicia el servidor, escuchando en el puerto definido en config.hrl (espera activa)
server() ->
    io:fwrite("Hola soy ~p~n", [self()]),
    {ok, ListenSocket} = gen_tcp:listen(?PORT, [{reuseaddr, true}, {active, true}]),
    io:fwrite("Escuchando peticiones ~n"),
    wait_connect(ListenSocket).

% Método de escucha del listener, al llegar conexioens nuevas las pasa a un proceso hijo para que las maneje.
wait_connect(ListenSocket) ->
    io:fwrite("~p se encarga de handlear a los clientes ~n", [self()]),
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    io:fwrite("Conexion desde ~p aceptada ~n", [Socket]),

    % Le transfiero "la propiedad" del socket al encargado de handlearlo. Sino, este nunca recibira los mensajes tcp
    gen_tcp:controlling_process(Socket, spawn (fun() -> handle_request(Socket) end)),
    wait_connect(ListenSocket).

% Envia el archivo (cuyo tamaño supera al umbral) en partes
send_chunks(Sock, BinData, FileSize, CantChunk, N) ->
    io:fwrite("iteracion ~p de ~p chunks~n", [N, CantChunk]),
    case N == (CantChunk - 1) of
        true -> 
            io:fwrite("Enviando chunk ~p de ~p bytes~n", [N, ?MAX_SINGLE_FILE_SIZE]),
            ChunkSize = FileSize - (?MAX_SINGLE_FILE_SIZE * (N)),
            ChunkData = binary:part(BinData, N * (?MAX_SINGLE_FILE_SIZE), ChunkSize),
            % siempre mandamos chunks de tamaño MAX_SINGLE_FILE_SIZE, excepto el ultimo que puede ser menor
            Chunk = <<
                (?CHUNK_CODE):8,
                N:16,
                ChunkSize:32/big-unsigned-integer,
                ChunkData/binary
            >>,
            gen_tcp:send(Sock, Chunk),
            ok;
        false ->
            io:fwrite("Enviando chunk ~p de ~p bytes~n", [N, ?MAX_SINGLE_FILE_SIZE]),
            ChunkData = binary:part(BinData, N * (?MAX_SINGLE_FILE_SIZE), ?MAX_SINGLE_FILE_SIZE),
            Chunk = <<
                (?CHUNK_CODE):8,
                N:16,
                (?MAX_SINGLE_FILE_SIZE):32/big-unsigned-integer,
                ChunkData/binary
            >>,

            case gen_tcp:send(Sock, Chunk) of
                ok -> 
                    io:fwrite("Chunk enviado! ~n"),
                    ok;
                {error, Reason} ->
                    io:fwrite("Error al enviar el chunk: ~p~n", [Reason]),
                    error
            end,
            send_chunks(Sock, BinData, FileSize, CantChunk, N + 1)
    end.

% Envía el archivo encontrado al cliente, si es menor al umbral lo envía de una, si es mayor lo envía en partes
send_found_file(Socket, FullPath, FileSize) ->

    case file:read_file(FullPath) of
        {error, Reason} ->
            io:format("Error al leer el archivo: ~p~n", [Reason]),
            gen_tcp:send(Socket, <<(?NOTFOUND_CODE):8>>),
            error;
        {ok, BinData} ->
            case FileSize < ?MAX_SINGLE_FILE_SIZE of
                true ->
                    case FileSize > 0 of
                        false ->
                            io:format("El archivo ~p no tiene contenido, no se enviara~n", [FullPath]),
                            % aca se manda notfound, aunque en realidad el error es otro
                            gen_tcp:send(Socket, <<(?NOTFOUND_CODE):8>>),
                            error;
                        true ->
                             % Si el archivo es menor al umbral, lo mando de una
                            Packet = <<
                                (?OK_CODE):8,
                                FileSize:32/big-unsigned-integer,
                                BinData/binary
                            >>,
                            io:format("Paquete definido: ~p~n", [Packet]),

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
                
                false ->
                    % Si el archivo es mayor al umbral, lo mando en partes
                    Packet = <<
                        (?OK_CODE):8,
                        FileSize:32/big-unsigned-integer,
                        (?MAX_SINGLE_FILE_SIZE):32/big-unsigned-integer
                    >>,

                    case gen_tcp:send(Socket, Packet) of
                        ok -> 
                            io:format("Paquete enviado! ~n"),
                            ok;
                        {error, Reason} ->
                            io:format("Error al enviar el paquete: ~p~n", [Reason]),
                            error
                    end,

                    CantChunks = ceil(FileSize / (?MAX_SINGLE_FILE_SIZE)),
                    io:format("Cant chunks: ~p~n", [CantChunks]),
                    send_chunks(Socket, BinData, FileSize, CantChunks, 0)
            end
    end.

% Maneja las conexiones entrantes, recibe los mensajes y responde a las peticiones
handle_request(Socket) ->
    io:fwrite("~p se encarga de handlear la conexion ~n", [self()]),
    receive
        {tcp, Cli, Data} ->
            Args = string:tokens(Data, " \n\r"),
            case Args of
                ["DOWNLOAD_REQUEST", FileName] ->
                    io:format("Download req: ~p ~n", [FileName]),
                    case file_lookup(FileName) of
                        #fileLookupSuccess{files = Files} ->
                            FullPath = (lists:nth(1, Files))#fileInfo.name,
                            FileSize = (lists:nth(1, Files))#fileInfo.size,

                            io:format("Archivo encontrado! ~n"),
                            send_found_file(Socket, FullPath, FileSize);
        
                        #fileLookupError{reason = Reason} ->
                            io:format("Archivo no encontrado: ~p ~n", [FileName]),
                            Packet = <<(?NOTFOUND_CODE):8>>,
                            gen_tcp:send(Socket, Packet),
                            error
                    end,
                    handle_request(Socket);
                ["QUIT"] ->
                    io:format("User quits~n"),
                    gen_tcp:close(Socket);
                ["SEARCH_REQUEST", Id, FileName] ->
                    file_gen:search_response(Socket, FileName);
                _Otherwise ->
                    gen_tcp:send(Cli, "ERROR: Comando desconocido\r\n"),
                    handle_request(Socket)

            end;
        {tcp_closed, Cli} ->
            io:format("Cliente ~p desconectado.~n", [Cli]),
            ok;

        _OtherMsg -> 
            io:format("Mensaje inesperado: ~p~n", [_OtherMsg]),
            handle_request(Socket)
    end.
