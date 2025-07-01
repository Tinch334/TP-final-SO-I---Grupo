-module(node).
-export([read_from_shared_folder/0, comm_handler/1, get_node_value/0, name_holder_loop/1, init/0]).
-include("config.hrl").

% Prints the elements of a list in an indexed and tabulated manner
pprint(List) ->
    IndList = lists:zip(lists:seq(1, length(List)), List),
    lists:foreach(fun({Ind, X}) -> io:format("~p. ~p ~n", [Ind, X]) end, IndList).

% It creates the shared and downloads directories if thei're not there
check_dirs() ->
    % Checking for shared
    case filelib:is_dir(?SHARED_PATH) of
        true ->
            io:format("La carpeta compartida esta presente ~n");
        false ->
            file:make_dir(?SHARED_PATH),
            io:format("Se creo la carpeta compartida~n")
    end,
    % Checking for downloads
    case filelib:is_dir(?DOWNLOADS_PATH) of
        true ->
            io:format("La carpeta descargas esta presente ~n");
        false ->
            file:make_dir(?DOWNLOADS_PATH),
            io:format("Se creo la carpeta descargas~n")
    end,
    ok.

% Reading the shared folder and returning a list with it's files
read_from_shared_folder() ->
    case file:list_dir(?SHARED_PATH) of
        {ok, RawFileNames} ->
            % It excludes, for example, symbolic links, folders, sockets, pipes, etc
            % The join is because is_regular receives paths, not only file names
            [F || F <- RawFileNames, filelib:is_regular(filename:join(?SHARED_PATH , F))];
        {error, Reason} ->
            io:format("Error leyendo directorio ~p: ~p ~n", [?SHARED_PATH], [Reason]),
            [] 
    end.

% Auxiliary function for the nteractive shell
shell() ->
    io:format("~s> ", [?SHELLNAME]),
    case io:get_line("") of
        eof ->
            io:format("Adios~n");
        {error, ErrorDescription} ->
            io:format("Error: ~p ~n", [ErrorDescription]),
            shell();
        Line ->
            if
                % If receiving a line, we derive it to the comm_handler, without the \n
                length(Line) > 1 -> comm_handler(hd(string:tokens(Line, "\n")));
                true -> ok
            end,
            shell()
    end.

% Manages the effects of the commands in the shell
comm_handler(Input) ->
    Args = string:tokens(Input, " "),
    case Args of
        ["EXIT"] ->
            % We exit the shell
            io:format("Adios~n"),
            halt();
        ["NODE_ID"] ->
            % Returns the ID of self
            io:format("~p ~n", [get_node_value()]);
        ["LIST_FILES"] ->
            % Lists the files from the shared folder
            pprint(read_from_shared_folder());
        ["SEARCH_REQUEST", FileName] ->
            % Sends a Search Request to the other nodes in the network, trying to find for files that 
            % matches with the received name (with wildcards)
            io:format("Buscando archivos... ~n"),
            case file_gen:search_request(FileName) of
                [] -> io:format("f se encontraron archivos~n");
                Lst -> lists:foreach(fun(E) -> io:format("ID: ~p - File: ~p - Size: ~p~n", [E#collectorElem.origId, E#collectorElem.filename, E#collectorElem.size]) end, Lst)
            end;
        ["DOWNLOAD_REQUEST", FileName, NodeIdStr] ->
            % Sends a Download Request to the specified node with the ID given
            % io:format("Intentando conectarse a ~p para descargar ~s...~n", [NodeIdStr, FileName]),
            case utils:get_info_from_id(NodeIdStr) of
                % If the node node isn't in the registry
                ?NOT_FOUND -> 
                    io:format("Id desconocida ~n");
                % If we receive the node information we try to connect and download
                Data -> 
                    % despues de 5s, dar timeout
                    io:format("Intentando conectar al node: ~p con IP ~p y puerto ~p ~n", [Data#nodeInfo.id, Data#nodeInfo.ip, Data#nodeInfo.port]),
                    case gen_tcp:connect(Data#nodeInfo.ip, list_to_integer(Data#nodeInfo.port), [binary, {packet, 0}, {active, false}], 5000) of    
                        {error, Reason} ->
                            io:format("Error al conectar con el node: ~p~n", [Reason]),
                            error;
                        % If connected, it tries to download
                        {ok, ConnSock} ->
                            io:format("Conectado a ~p~n", [ConnSock]),

                            % Checking the result of the download
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
            % Lists the nodes from the registry
            % io:format("~p ~n", [utils:get_nodes_from_registry()]);
                
            L = utils:get_nodes_from_registry(),
            case L of
                [] -> io:format("No se conocen otros nodes~n");
                Lst -> 
                    lists:foreach(fun(E) -> io:format("-~p, ~p, ~p ~n", [E#nodeInfo.id, E#nodeInfo.ip, E#nodeInfo.port]) end, Lst),
                    io:format("Peers: ~p ~n", [length(Lst)])
            end;
        ["HELP"] ->
            % Shows the available shell commands
            pprint(?SHELL_COMMS);
        ["REFRESH"] ->
            utils:make_node_registry();
        ["IP_MAP", Ip] ->
            utils:ip_checker(Ip); 

        _ ->
            % Unknown command
            io:format("Comando desconocido. 'HELP' para ver los comandos disponibles y su uso. ~n")
    end.

% Save node name as a getter
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
