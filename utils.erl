-module(utils).
-export([file_lookup/1]).

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