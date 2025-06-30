-module(file_gen).
-export([search_request/1, search_response/2, collector/1, search_handler/3]).

-include("gen_header.hrl").
-include("config.hrl").

%Section 4.2, created as general library for common use.
search_request(Filename) ->
    CollectorID = spawn(?MODULE, collector, [[]]),
    create_handlers(CollectorID, Filename, utils:get_nodes_from_registry()),%Get node list
    timer:sleep(?SEARCH_REQUEST_TIMEOUT + 500), %Wait for TCP timeout.
    CollectorID ! {?COLLECTOR_GET, self()},
    receive
        {collectorRes, Lst} -> Lst
    end.

collector(Lst) ->
    receive
        #collectorElem{origId = OID, filename = Filename, size = Size} ->
            collector([#collectorElem{origId = OID, filename = Filename, size = Size} | Lst]);
        {?COLLECTOR_GET, Id} -> Id ! {collectorRes, Lst} %We don't use a record for an internal one off.
    end.

create_handlers(_, _, []) -> [];
create_handlers(CId, Filename, [Node | NodeLst]) ->
    timer:kill_after(?SEARCH_REQUEST_TIMEOUT, spawn(?MODULE, search_handler, [CId, Filename, Node])),
    create_handlers(CId, Filename, NodeLst).

search_handler(CId, Filename, Node) ->
    case gen_tcp:connect(Node#nodeInfo.ip, Node#nodeInfo.port, [inet]) of
        {ok, Socket} ->
            gen_tcp:send(Socket, lists:concat(["SEARCH_REQUEST", " ", myid, " ", Filename])),
            search_handler_recv(CId),
            gen_tcp:close(Socket);
        {error, Reason} ->
            io:format("An error occurred creating a TCP file request socket, with error: ~w~n", [Reason])
    end.

search_handler_recv(CId) ->
    receive
        {tcp, Socket, Data} ->
            SeparatedData = string:tokens(Data, " "),
            CId ! #collectorElem{origId = lists:nth(1, SeparatedData), filename = lists:nth(2, SeparatedData), size = lists:nth(3, SeparatedData)},
            gen_tcp:close(Socket);
        {error, Reason} -> io:format("An error occurred reading from a TCP file request socket, with error: ~w~n", [Reason])
    end.


search_response(Socket, FileName) ->
    case utils:file_lookup(FileName) of
        #fileLookupSuccess{files = FileLst} -> send_file_info(Socket, FileLst);
        #fileLookupError{reason = _} -> ok
    end.

send_file_info([], _) -> ok;
send_file_info(Socket, [File | Files]) ->
    FileMsg = lists:concat(["SEARCH_RESPONSE", " ", myid, " ", File#fileInfo.name, " ", File#fileInfo.size]),
    gen_tcp:send(Socket, FileMsg),
    send_file_info(Socket, Files).