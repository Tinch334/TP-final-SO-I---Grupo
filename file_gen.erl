-module(file_gen).
-export([search_request/1, search_response/2]).

-include("gen_header.hrl").

%Section 4.2, created as general library for common use.
search_request(Filename) ->
    CollectorID = spawn(?MODULE, collector, [[]]),
    create_handlers(CollectorID, Filename, []),%Get node list
    timer:sleep(?SEARCH_REQUEST_TIMEOUT) %Wait for TCP timeout.
    CollectorID ! ?COLLECTOR_GET.

collector(Lst) ->
    receive
        #collectorElem{origId = OID, filename = Filename, size = Size} ->
            collector [#collectorElem{origId = OID, filename = Filename, size = Size} | Lst];
        ?COLLECTOR_GET -> LST
    end.

create_handlers(CId, _, []) -> [].
create_handlers(CId, Filename, [Node | NodeLst]) ->
    spawn(?MODULE, search_handler, [CId, Filename, Node]),
    create_handlers(Filename, NodeLst).

search_handler(CId, Filename, Node) ->
    case gen_tcp:connect(Node#nodeInfo.addr, Node#nodeInfo.port, [inet], ?SEARCH_REQUEST_TIMEOUT) ->
        {ok, Socket} -> gen_tcp:send{Socket, lists:concat(["SEARCH_REQUEST", " ", myid, " ", Filename])}
        {error, Reason} -> io:format("An error occurred creating a TCP file request socket, with error: ~w~n", [Reason])
    end,
    search_handler_recv(CId).

search_handler_recv(CId) ->
    receive
        {tcp, Socket, Data} ->
            SeparatedData = string:tokens(Data, " "),
            CId ! #collectorElem{origId = lists:nth(1, SeparatedData), filename = lists:nth(2, SeparatedData), size = lists:nth(3, SeparatedData)};
        {error, Reason} -> io:format("An error occurred reading from a TCP file request socket, with error: ~w~n", [Reason])
    end.


search_response(Socket, Filename) ->
    FileList = [],%Get files
    send_file_info(Socket, FileList).

send_file_info([], _) ->
    ok.
send_file_info(Socket, [File | Files]) ->
    FileMsg = lists:concat(["SEARCH_RESPONSE", " ", myid, " ", File#fileInfo.name, " ", File#fileInfo.size]),
    gen_tcp:send(Socket, FileMsg),
    send_file_info(Socket, Files).