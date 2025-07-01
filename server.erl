-module(server).
-export([server/0]).
-include("config.hrl").

% Starts the server, listening to the defined port used in config.hrl (active wait)
server() ->
    % io:fwrite("Hola soy ~p~n", [self()]),
    {ok, ListenSocket} = gen_tcp:listen(?PORT, [{reuseaddr, true}, {active, true}]),
    io:fwrite("Listening to requests... ~n"),
    wait_connect(ListenSocket).

% Listening method, when new connections come it creates a child proccess to handle it.
wait_connect(ListenSocket) ->
    % io:fwrite("~p se encarga de handlear a los clientes ~n", [self()]),
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    % io:fwrite("Conexion desde ~p aceptada ~n", [Socket]),

    % Transfers the "property" of the socket to the procces that is in charge of it. If not, this never recieves tcp messages
    gen_tcp:controlling_process(Socket, spawn (fun() -> handle_request(Socket) end)),
    wait_connect(ListenSocket).

% Handles incoming connections, gets the messages and answers queries
handle_request(Socket) ->
    % io:fwrite("~p se encarga de handlear la conexion ~n", [self()]),
    receive
        {tcp, Cli, Data} ->
            Args = string:tokens(Data, " \n\r"),
            case Args of
                ["DOWNLOAD_REQUEST", FileName] ->
                    %io:format("Download req: ~p ~n", [FileName]),
                    case utils:file_lookup(FileName) of
                        #fileLookupSuccess{files = Files} ->
                            FullPath = (lists:nth(1, Files))#fileInfo.name,
                            FileSize = (lists:nth(1, Files))#fileInfo.size,

                            io:format("Archivo encontrado! ~n"),
                            spawn(fun() -> downloader:send_found_file(Socket, FullPath, FileSize) end);

                        #fileLookupError{reason = _} ->
                            io:format("Archivo no encontrado: ~p ~n", [FileName]),
                            Packet = <<(?NOTFOUND_CODE):8>>,
                            gen_tcp:send(Socket, Packet),
                            error
                    end,
                    handle_request(Socket);
                ["QUIT"] ->
                    %io:format("User quits~n"),
                    gen_tcp:close(Socket);
                ["SEARCH_REQUEST", _, FileName] ->
                    file_gen:search_response(Socket, FileName),
                    handle_request(Socket);
                _Otherwise ->
                    gen_tcp:send(Cli, "ERROR: Comando desconocido\r\n"),
                    handle_request(Socket)
            end;
        {tcp_closed, _Cli} ->
            % io:format("Cliente ~p desconectado.~n", [Cli]),
            ok;

        _OtherMsg -> 
            io:format("Mensaje inesperado: ~p~n", [_OtherMsg]),
            handle_request(Socket)
    end.