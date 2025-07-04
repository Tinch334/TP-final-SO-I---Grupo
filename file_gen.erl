-module(file_gen).
-export([search_request/1, search_response/2, collector/1, search_handler/3]).
-include("config.hrl").


% Process the search request command, then waits for every message to be recieved through the collector.
% after the timeout it sends a special message to the collector to retrieve the result of the search
search_request(Filename) ->
    CollectorID = spawn(?MODULE, collector, [[]]),
    create_handlers(CollectorID, Filename, utils:get_nodes_from_registry()),% Get node list.
    timer:sleep(?SEARCH_REQUEST_TIMEOUT + 500), % Wait for TCP timeout, 500ms are added to be sure that it goes after the kill signal from create_handlers
    CollectorID ! {?COLLECTOR_GET, self()},
    receive
        #collectorRes{lst = Lst} -> Lst
    end.

% Listens and compiles the data of the search in Lst, when th message {?COLLECTOR_GET, Id} is recieved it sends all the data cllected to Id
collector(Lst) ->
    receive
        #collectorElem{origId = OID, filename = Filename, size = Size} ->
            collector([#collectorElem{origId = OID, filename = Filename, size = Size}] ++ Lst);
        {?COLLECTOR_GET, Id} -> Id ! #collectorRes{lst = Lst} % We don't use a record for an internal one off.
    end.

% creates the search_handlers for each node in the internal list, and sets the timeout of every one of them
create_handlers(_, _, []) -> [];
create_handlers(CId, Filename, [Node | NodeLst]) ->
    timer:kill_after(?SEARCH_REQUEST_TIMEOUT, spawn(?MODULE, search_handler, [CId, Filename, Node])),
    create_handlers(CId, Filename, NodeLst).

% it connects through tcp to the given node and send it a search request in the specified format, then calls search_handler_recv to listen to the answer
search_handler(CId, Filename, Node) ->
    case gen_tcp:connect(Node#nodeInfo.ip, list_to_integer(Node#nodeInfo.port), [inet]) of
        {ok, Socket} ->
            gen_tcp:send(Socket, lists:concat(["SEARCH_REQUEST", " ", node:get_node_value() , " ", Filename, "\n"])),
            search_handler_recv(CId);
        {error, Reason} ->
            io:format("Ocurrio un error al conectarse a un node, debido a: ~w~n", [Reason])
    end.

% does a recieve to get the data, separates it in files (more than one file can ve recieved at once) and calls make_collector_elem for every file,
search_handler_recv(CId) ->
    receive
        {tcp, _, Data} ->
            SeparatedData = string:tokens(Data, "\n"),
            lists:foreach(fun(Line) -> make_collector_elem(Line, CId) end, SeparatedData),
            search_handler_recv(CId);
        {error, Reason} -> io:format("Ocurrio un error al comunicarse con un node, debido a: ~w~n", [Reason])
    end.

% Separates a string differentiating through a space, then "SEARCH_RESPONSE" is the first element and all the other ones are specified 
% in the collectorElem record
make_collector_elem(Line, CId) ->
    SeparatedMsg = string:tokens(Line, " "),
    Len = length(SeparatedMsg),
    %% io:format("len: ~p ~n", [Len]),
    %% parsing err
    case (Len >= 4) of
        true ->
            CId ! #collectorElem{origId = lists:nth(2, SeparatedMsg), filename = lists:nth(3, SeparatedMsg), size = lists:nth(4, SeparatedMsg)};
        false -> 
            ok
    end.



% Handles the response when another node asks this one for files, when there are files it sends them to send_file_info to make a response
search_response(Socket, FileName) ->
    case utils:file_lookup(FileName) of
        #fileLookupSuccess{files = FileLst} -> send_file_info(Socket, FileLst);
        #fileLookupError{reason = _} -> ok
    end.

% From a list of files, for each one of them it makes a consensus approved response and sends it to the specified socket
send_file_info(_, []) -> ok;
send_file_info(Socket, [File | Files]) ->
    FileMsg = lists:concat(["SEARCH_RESPONSE", " ", node:get_node_value(), " ", File#fileInfo.name, " ", File#fileInfo.size, "\n"]),
    gen_tcp:send(Socket, FileMsg),
    send_file_info(Socket, Files).