-module(nodo).
-export([init/0, read_from_shared_folder/0, pprint/1, shell/0, comm_handler/1, get_node_value/0, name_holder_loop/1, name_generator/1]).
-include("config.hrl").
-include("gen_header.hrl").

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
            io:format("Adios~n"),
            halt();
        ["id_nodo"] ->
            io:format("~p ~n", [get_node_value()]);
        ["listar_mis_archivos"] ->
            pprint(read_from_shared_folder());
        ["SEARCH_REQUEST", FileName] ->
            io:format("Archivos encontrados: ~w~n", [file_gen:search_request(FileName)]);
        ["DOWNLOAD_REQUEST", FileName, NodeIdStr] ->
            io:format("Intentando conectarse a ~p para descargar ~s...~n", [NodeIdStr, FileName]),
            case utils:get_info_from_id(NodeIdStr) of
                notFound -> 
                    io:format("Id desconocida ~n");
                Data -> 
                    % despues de 5s, dar timeout
                    case gen_tcp:connect(Data#nodeInfo.ip, Data#nodeInfo.port, [binary, {packet, 0}, {active, false}], 5000) of
                        
                        {error, Reason} ->
                            io:format("Error al conectar con el nodo: ~p~n", [Reason]),
                            error;
                        %% si se pudo conectar, descargar el archivo
                        {ok, ConnSock} ->
                            io:format("Conectado a ~p~n", [ConnSock]),
                            case downloader:download_file(FileName, ConnSock) of
                                {error, closed} ->
                                    io:format("Conexion cerrada antes de completar la descarga~n");
                                {error, not_found} ->
                                    io:format("Error: archivo no encontrado en el server~n");
                                {error, empty_file} ->
                                    io:format("Error: el archivo ~s esta vacio~n", [FileName]);
                                {error, Reason} ->
                                    io:format("Error: Ocurrio un error inesperado: ~w~n", [Reason]);
                                ok ->
                                    gen_tcp:close(ConnSock),
                                    io:format("Archivo descargado! ~n")
                            end
                        end
                end;
        ["help"] ->
            pprint(?SHELL_COMMS);
        _ ->
            io:format("Comando desconocido. 'help' para ver los comandos disponibles y su uso. ~n")
    end.

% Save node name.
name_holder_loop(Value) ->
    receive
        {get_value, Caller} ->
            Caller ! {value, Value},
            name_holder_loop(Value)
    end.

% Get node name via message passing.
get_node_value() ->
    name_holder ! {get_value, self()},
    receive
        {value, V} -> V
    end.

% Initializes the node, checks directories, reads shared files, and starts the UDP listener.
init() ->
    io:format("Inicializa nodo ~n"),
    check_dirs(),
    _Shr = read_from_shared_folder(),
    _Discovered = [],
    io:format("Archivos compartidos: ~p ~n", [?TEST_NAMES]),

    udp_gen:gen_udp_init(),

    register(tcp_server, spawn(fun() -> server:server() end)),


    shell(),

    % probablemente convenga dejar el server tcp registrado haciendole spawn con otro proceso
    % server:server(),
    ok.
