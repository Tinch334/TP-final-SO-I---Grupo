-module(test).
-export([add_node_to_registry/3, get_nodes_from_registry/0]).

-include("config.hrl").

add_node_to_registry(Ip, Id, Port) ->
    Line = io_lib:format("~s,~s,~s~n", [Ip, Id, Port]),
    file:write_file(?REG_PATH, Line, [append]). % append it at the end of the file (no overwriting)

get_nodes_from_registry() ->
    case file:read_file(?REG_PATH) of
        {ok, Data} ->
            StringList = lists:map(fun(L) -> binary_to_list(L) end, binary:split(Data, [<<"\r\n">>], [global])),
            lists:map(make_node_record, StringList);
        {error, _} -> []
    end.

make_node_record(NodeString) ->
    Split = string:tokens(NodeString, ","),
    #nodeInfo{ip = lists:nth(1, Split), id = lists:nth(2, Split), port = lists:nth(3, Split)}.