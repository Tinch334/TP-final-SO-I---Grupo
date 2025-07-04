% subir esto
-module(utils).
-export([file_lookup/1, file_lookup_fullpath/1, add_node_to_registry/3, get_nodes_from_registry/0,get_clean_filename/1, make_node_record/1, ip_checker/1, id_in_registry/1, get_info_from_id/1, make_node_registry/0]).
-include("config.hrl").

% removes the shared folder name from path
get_clean_filename(Path) ->
    lists:nth(3, string:replace(Path, "compartida/", "", all)).

file_lookup_fullpath(FileName) ->
    FullSharedPath = filename:join(?SHARED_PATH, FileName),

    % Searching with wildcards in the shared directory
    case filelib:wildcard(FullSharedPath) of
        [] ->
            #fileLookupError{reason = "File not found"};

        Lst ->
            % Returning the file information of the found files
            #fileLookupSuccess{files = generate_fileinfo_fullpath(Lst)}
    end.

% File lookup function, given a name it searches the downloads and shared directories for it
file_lookup(FileName) ->
    FullSharedPath = filename:join(?SHARED_PATH, FileName),

    % Searching with wildcards in the shared directory
    case filelib:wildcard(FullSharedPath) of
        [] ->

            #fileLookupError{reason = "File not found"};

        Lst ->
            % Returning the file information of the found files
            io:format("lst: ~p ~n", [Lst]),
            #fileLookupSuccess{files = generate_fileinfo(Lst)}
    end.

% It generates a list of fileInfo records
generate_fileinfo([]) -> [];
generate_fileinfo([File | FileLst]) ->
    [#fileInfo{name = utils:get_clean_filename(File), size = filelib:file_size(File)} | generate_fileinfo(FileLst)].

% Does the same as funcion above, but using full path
generate_fileinfo_fullpath([]) -> [];
generate_fileinfo_fullpath([File | FileLst]) ->
    [#fileInfo{name = File, size = filelib:file_size(File)} | generate_fileinfo(FileLst)].

% Makes the node_registry file for the record of nodes in the network
make_node_registry() ->
    file:write_file(?NODE_FILE, <<>>).

% Check if node id is already in registry
id_in_registry(Val) ->
    L = get_nodes_from_registry(),
    lists:any(fun(#nodeInfo{ip=_, id=Id, port=_}) -> Id =:= Val end, L).

% Gets the info from a node in our node_registry if it's in it
get_info_from_id(Id) ->
    case id_in_registry(Id) of
        true ->
            L = get_nodes_from_registry(),
            lists:nth(1, lists:filter(fun(#nodeInfo{ip=_, id=IdRes, port=_}) -> Id == IdRes end, L));
        false -> 
            ?NOT_FOUND
    end.

% Add data about discovered nodes to the registry file, if they aren't added already
add_node_to_registry(Ip, Id, Port) ->
    case (id_in_registry(Id) or (Id == node:get_node_value()) ) of 
        true -> ok;
        
        false ->
            Line = io_lib:format("~s,~s,~s~n", [inet:ntoa(Ip), Id, Port]),
            file:write_file(?NODE_FILE, Line, [append]) % append it at the end of the file (no overwriting)
    end.

% Returns a list of node information records form the node_registry
get_nodes_from_registry() ->
    case file:read_file(?NODE_FILE) of
        {ok, Data} ->
            InfoList = lists:map(fun binary_to_list/1, binary:split(Data, [<<"\n">>], [global])),
            ValidLines = lists:filter(fun(Line) -> Line =/= "" end, InfoList), % remove empty lines
            lists:map(fun make_node_record/1, ValidLines);
        {error, _} ->
            []
    end.

% Given the string of a line in the node_registry it returns a record of the node information
make_node_record(NodeString) ->
    Split = string:tokens(NodeString, ","),
    #nodeInfo{ip = lists:nth(1, Split), id = lists:nth(2, Split), port = lists:nth(3, Split)}.


ip_in_registry(IpVal) ->
    L = get_nodes_from_registry(),
    lists:any(fun(#nodeInfo{ip=Ip, id=_, port=_}) -> Ip =:= IpVal end, L).


ip_checker(Ip) ->
    io:format("Check nodes linked with ip: ~p ~n", [Ip]),
    case ip_in_registry(Ip) of
        true ->
            L = get_nodes_from_registry(),
            Filtered = lists:filter(fun(#nodeInfo{ip=IpRes, id=_, port=_}) -> Ip == IpRes end, L),
            Res = lists:map(fun(Tup) -> element(3, Tup) end, Filtered),
            io:format("~p ~n", [Res]);
        false -> 
            io:format("No encontro ningun peer vinculado a esa IP ~n")
    end.