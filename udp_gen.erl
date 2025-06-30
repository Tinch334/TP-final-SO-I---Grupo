%Section 4.1, created as general library for common use.
-module(udp_gen).
-export([gen_udp_init/0, hello_sender_init/3, hello_sender/3, namereq_sender/3, udp_discover_listener/1, handle_udp_req/3, choose_name/2]).

-include("gen_header.hrl").
-include("config.hrl").


id_holder_loop(Dict) ->
    receive
        {get_value, Caller, Id} ->
            try dict:fetch(Id, Dict) of
                Response -> Caller ! {ok, Response}
            catch
                _ -> notFound
            end,
            id_holder_loop(Dict)
    end.

get_id_value(Id) ->
    id_holder ! {get_value, self(), Id},
    receive
        {ok, Response} -> {ok, Response};
        notfound -> notFound
    end.

% listen for incoming UDP requests
udp_discover_listener(Sock) ->
    case gen_udp:recv(Sock, 0) of
        {ok, {Ip, _Port, Str}} ->
            io:format("Received UDP message: ~p from ~p~n", [Str, Ip]),
            handle_udp_req(Str, Ip, Sock);
        {error, Reason} ->
            io:format("Error al recibir UDP: ~p~n", [Reason]),
            udp_discover_listener(Sock)
    end.

% handle the request received from UDP socket
handle_udp_req(Str, IpFrom, Sock) ->
    case string:tokens(Str, " \n") of
        ["NAME_REQUEST", IdStr] ->
            io:format("NAME_REQUEST from ~p with id: ~p~n", [IpFrom, IdStr]),
            Id = list_to_integer(IdStr),
            NV = nodo:get_node_value(),
            % TODO: verificar si: El mismo id ya fue solicitado en una NAME_REQUEST enviada por este nodo anteriormente.
            case Id =/= NV of
                true ->
                    case get_id_value(Id) of
                        {ok, Response} ->
                            case Id == Response of
                                true ->
                                    io:format("NAME_REQUEST with previously used id!, invalidate.~n"),
                                    Sock ! #udpSend{addr = IpFrom, port = ?UDP_SOCKET, msg = string:join(["INVALID_NAME", IdStr, "\n"], " ")},
                                    ok;
                                false -> 
                                    io:format("NAME_REQUEST from another node: ~p~n", [Id]),
                                    ok
                            end;
                        notfound ->
                            io:format("NAME_REQUEST from another node: ~p~n", [Id]),
                            ok
                    end;
                false ->
                    io:format("NAME_REQUEST with my own id!, invalidate.~n"),
                    Sock ! #udpSend{addr = IpFrom, port = ?UDP_SOCKET, msg = string:join(["INVALID_NAME", IdStr, "\n"], " ")},
                    ok
                    %% enviar invalid
            end,
            udp_discover_listener(Sock);

        ["HELLO", IdStr, PortStr] ->
            io:format("HELLO from ~p with id: ~p and port: ~p~n", [IpFrom, IdStr, PortStr]),
            io:format("add ~p ~p ~p to register~n", [IpFrom, IdStr, PortStr]),
            utils:add_node_to_registry(IpFrom, IdStr, PortStr),
            udp_discover_listener(Sock);

        _ ->
            io:format("Mensaje desconocido: ~p~n", [Str]),
            udp_discover_listener(Sock)
    end.

% follows the protocol to choose a unique name for the node. precond: an active UDP connection is open
choose_name(Sock, Timeout) ->
    NameChoice = nodo:name_generator(?NODE_NAME_LENGTH),
    namereq_sender(self(), NameChoice, Sock), % send name request
    io:format("Sending name: ~p~n", [NameChoice]),
    wait_for_invalid_name(Sock, NameChoice, Timeout).

wait_for_invalid_name(Sock, NameChoice, RemainingTime) ->
    case (RemainingTime =< 0) of
        true -> 
            io:format("Time's up. Name is accepted. ~p~n", [NameChoice]),
            NameChoice;
        false ->
            Start = erlang:monotonic_time(millisecond), % start counting time
            case gen_udp:recv(Sock, 0, RemainingTime) of
                {ok, {_Addr, _Port, Data}} ->
                    case Data of
                        {"INVALID_NAME", _ID} ->
                            io:format("Name ~p already in use!~n", [NameChoice]),
                            timer:sleep(rand:uniform(8000) + 2000),  % wait a random time between 2s-10s and retry
                            choose_name(Sock, 10000); 
                        _Other ->
                            Elapsed = erlang:monotonic_time(millisecond) - Start, % stop counting, wait again but less
                            NewRemaining = RemainingTime - Elapsed,
                            wait_for_invalid_name(Sock, NameChoice, NewRemaining)
                    end;
                {error, timeout} ->
                    io:format("Timeout interno: aceptamos nombre ~p~n", [NameChoice]),
                    NameChoice;
                {error, Reason} ->
                    io:format("Error inesperado: ~p. Reintentando…~n", [Reason]),
                    choose_name(Sock, 10000)
            end
    end.


% initializes the UDP socket and concurrently starts the hello sender and udp listener processes.
gen_udp_init() ->
    case gen_udp:open(?UDP_SOCKET, [list, {active, false}, {reuseaddr, true}, {broadcast, true}]) of
        {error, Reason} -> 
            io:format("Error creating UDP socket: [~p] ~n", [Reason]);
        {ok, UDPConnSock} ->
            io:format("UDP sucessfully created~n"),
            NodeName = choose_name(UDPConnSock, 10000), % get unique name for the node
            register(name_holder, spawn(fun() -> nodo:name_holder_loop(NodeName) end)), % launch the name holder process
            register(id_holder, spawn(fun() -> id_holder_loop(dict:new()) end)),
            register(?UDP_SENDER_ID, spawn(fun() -> udp_gen:hello_sender_init(self(), NodeName, UDPConnSock), receive after infinity -> ok end  end)) , % periodically send HELLO
            register(?UDP_RECEIVER_ID, spawn(fun() -> udp_discover_listener(UDPConnSock) end)), % listen for incoming UDP requests
            io:format("Mi nombre es: ~p~n", [nodo:get_node_value()])
    end.


%Periodically sends a HELLO message with the specified format using a UDP broadcast. Requires the TCP socket and the node ID.
hello_sender_init(Sender, Id, Sock) ->
    io:format("Starting hello sender with ID ~p~n", [Id]),
    timer:apply_interval(?HELLO_INTERVAL, ?MODULE, hello_sender, [Sender, Id, Sock]).
    
hello_sender(Sender, IdStr, Sock) ->
    io:format("~nSEND HI ~n"),
    Msg = ["HELLO ", IdStr, " ", integer_to_list(?PORT), "\n"],
    gen_udp:send(Sock, {255, 255, 255, 255}, (?UDP_SOCKET), Msg).

% Sends a name request to the broadcast address with the specified ID.
namereq_sender(Sender, IdStr, Sock) ->
    Msg = ["NAME_REQUEST ", IdStr, "\n"],
    gen_udp:send(Sock, {255, 255, 255, 255}, (?UDP_SOCKET), Msg).

% no hace el broadcast

