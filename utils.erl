-module(utils).
-export([file_lookup/1, add_node_to_registry/3, get_nodes_from_registry/0, make_node_record/1]).

-include("config.hrl").

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


% Add data about discovered nodes to the registry file.
add_node_to_registry(Ip, Id, Port) ->
    Line = io_lib:format("~s,~s,~s~n", [Ip, Id, Port]),
    file:write_file(?REG_PATH, Line, [append]). % append it at the end of the file (no overwriting)

get_nodes_from_registry() ->
    case file:read_file(?REG_PATH) of
        {ok, Data} ->
            StringList = lists:map(fun(L) -> binary_to_list(L) end, binary:split(Data, [<<"\r\n">>], [global])),
            lists:map(fun(E) -> make_node_record(E) end, StringList);
        {error, _} -> []
    end.

make_node_record(NodeString) ->
    Split = string:tokens(NodeString, ","),
    #nodeInfo{ip = lists:nth(1, Split), id = lists:nth(2, Split), port = lists:nth(3, Split)}.