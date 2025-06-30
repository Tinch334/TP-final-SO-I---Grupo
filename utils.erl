-module(utils).
-export([file_lookup/1, add_node_to_registry/3, get_nodes_from_registry/0, make_node_record/1, id_in_registry/1]).

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

% check if node id is already in registry
id_in_registry(Val) ->
    L = get_nodes_from_registry(),
    lists:any(fun(#nodeInfo{ip=_, id=Id, port=_}) -> Id =:= Val end, L).

% Add data about discovered nodes to the registry file, if they aren't added already
add_node_to_registry(Ip, Id, Port) ->
    case (id_in_registry(Id)) of 
        true -> ok;
        
        false ->
            Line = io_lib:format("~s,~s,~s~n", [inet:ntoa(Ip), Id, Port]),
            file:write_file(?REG_PATH, Line, [append]) % append it at the end of the file (no overwriting)
    end.

get_nodes_from_registry() ->
    case file:read_file(?REG_PATH) of
        {ok, Data} ->
            InfoList = lists:map(fun binary_to_list/1, binary:split(Data, [<<"\n">>], [global])),
            ValidLines = lists:filter(fun(Line) -> Line =/= "" end, InfoList), % remove empty lines
            lists:map(fun make_node_record/1, ValidLines);
        {error, _} ->
            []
    end.

make_node_record(NodeString) ->
    Split = string:tokens(NodeString, ","),
    #nodeInfo{ip = lists:nth(1, Split), id = lists:nth(2, Split), port = lists:nth(3, Split)}.