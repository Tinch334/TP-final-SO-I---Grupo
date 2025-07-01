%TODO: Explain what each file does
-module(node).
-export([read_from_shared_folder/0, comm_handler/1, get_node_value/0, name_holder_loop/1, init/0]).
-include("config.hrl").

% nc localhost 12345 (o alternativamente compilar node y descargar desde la cli)

% Imprime de manera indexada y tabulada los elementos de una lista
pprint(List) ->
    IndList = lists:zip(lists:seq(1, length(List)), List),
    lists:foreach(fun({Ind, X}) -> io:format("~p. ~p ~n", [Ind, X]) end, IndList).

% Crea las carpetas del node si no están creadas.
check_dirs() ->
    case filelib:is_dir(?SHARED_PATH) of
        true ->
            io:format("La carpeta compartida esta presente ~n");
        false ->
            file:make_dir(?SHARED_PATH),
            io:format("Se creo la carpeta compartida~n")
    end,
    case filelib:is_dir(?DOWNLOADS_PATH) of
        true ->
            io:format("La carpeta descargas esta presente ~n");
        false ->
            file:make_dir(?DOWNLOADS_PATH),
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

% Funcion auxiliar usada por la shell interactiva
shell() ->
    io:format("~s> ", [?SHELLNAME]),
    case io:get_line("") of
        eof ->
            io:format("Adios~n");
        {error, ErrorDescription} ->
            io:format("Error: ~p ~n", [ErrorDescription]),
            shell();
        Line ->
            % pasarle la linea, quitandole el enter
            if
                length(Line) > 1 -> comm_handler(hd(string:tokens(Line, "\n")));
                true -> ok
            end,
            shell()
    end.

% Evaluar comandos
comm_handler(Input) ->
    Args = string:tokens(Input, " "),
    case Args of
        ["EXIT"] ->
            io:format("Adios~n"),
            halt();
        ["NODE_ID"] ->
            io:format("~p ~n", [get_node_value()]);
        ["LIST_FILES"] ->
            pprint(read_from_shared_folder());
        ["SEARCH_REQUEST", FileName] ->
            io:format("Buscando archivos... ~n"),
            case file_gen:search_request(FileName) of
                [] -> io:format("No se encontraron archivos~n");
                Lst -> lists:foreach(fun(E) -> io:format("ID: ~p - File: ~p - Size: ~p~n", [E#collectorElem.origId, E#collectorElem.filename, E#collectorElem.size]) end, Lst)
            end;
        ["DOWNLOAD_REQUEST", FileName, NodeIdStr] ->
            io:format("Intentando conectarse a ~p para descargar ~s...~n", [NodeIdStr, FileName]),
            case utils:get_info_from_id(NodeIdStr) of
                ?NOT_FOUND -> 
                    io:format("Id desconocida ~n");
                Data -> 
                    % despues de 5s, dar timeout
                    io:format("Intentando conectar al node: ~p con IP ~p~n", [Data#nodeInfo.id, Data#nodeInfo.ip]),
                    case gen_tcp:connect(Data#nodeInfo.ip, list_to_integer(Data#nodeInfo.port), [binary, {packet, 0}, {active, false}], 5000) of    
                        {error, Reason} ->
                            io:format("Error al conectar con el node: ~p~n", [Reason]),
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
        ["LIST_NODES"] ->
            case utils:get_nodes_from_registry() of
                [] -> io:format("No se conocen otros nodes~n");
                Lst -> lists:foreach(fun(E) -> io:format("-~p~n", [E#nodeInfo.id]) end, Lst)
            end;
        ["HELP"] ->
            pprint(?SHELL_COMMS);
        _ ->
            io:format("Comando desconocido. 'HELP' para ver los comandos disponibles y su uso. ~n")
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
    ?NODE_NAME_HOLDER ! {get_value, self()},
    receive
        {value, V} -> V
    end.

% Initializes the node, checks directories, reads shared files, and starts the UDP listener.
init() ->
    %io:format("Inicializa node ~n"),
    utils:make_node_registry(),
    check_dirs(),

    udp_gen:gen_udp_init(),

    register(tcp_server, spawn(fun() -> server:server() end)),


    shell(),
    ok.
