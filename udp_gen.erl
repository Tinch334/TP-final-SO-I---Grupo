-module(udp_gen).
-export([gen_udp_init/0, hello_sender_init/2, hello_sender/2, namereq_sender/2, udp_discover_listener/1, handle_udp_req/3, choose_name/2]).
-include("config.hrl").

% Listen for incoming UDP requests
udp_discover_listener(Sock) ->
    case gen_udp:recv(Sock, 0) of
        {ok, {Ip, _Port, Str}} ->
            %io:format("Received UDP message: ~p from ~p~n", [Str, Ip]),
            handle_udp_req(Str, Ip, Sock);
        {error, Reason} ->
            io:format("Error al recibir UDP: ~p~n", [Reason]),
            udp_discover_listener(Sock)
    end.

% Handle the request received from UDP socket
handle_udp_req(Str, IpFrom, Sock) ->
    case string:tokens(Str, " \n") of
        ["NAME_REQUEST", IdStr] ->
            % io:format("NAME_REQUEST from ~p with id: ~p~n", [IpFrom, IdStr]),
            Id = IdStr,
            NV = node:get_node_value(),
            case Id =/= NV of % Checks if the new node has my name
                true ->
                    case utils:id_in_registry(Id) of
                        true ->
                            io:format("NAME_REQUEST with previously used id!, invalidate.~n"),
                            gen_udp:send(Sock, IpFrom, (?UDP_SOCKET), string:join(["INVALID_NAME", IdStr, "\n"], " "));
                        false ->
                            ok
                            % io:format("NAME_REQUEST from another node: ~p~n", [Id])
                    end;
                false ->
                    io:format("NAME_REQUEST with my own id!, invalidate.~n"),
                    gen_udp:send(Sock, IpFrom, (?UDP_SOCKET), string:join(["INVALID_NAME", IdStr, "\n"], " "))
            end,
            udp_discover_listener(Sock);

        ["HELLO", IdStr, PortStr] ->
            %io:format("HELLO from ~p with id: ~p and port: ~p~n", [IpFrom, IdStr, PortStr]),
            %io:format("add ~p ~p ~p to register~n", [IpFrom, IdStr, PortStr]),
            utils:add_node_to_registry(IpFrom, IdStr, PortStr),
            udp_discover_listener(Sock);

        _ ->
            io:format("Mensaje desconocido: ~p~n", [Str]),
            udp_discover_listener(Sock)
    end.

% Follows the protocol to choose a unique name for the node, an active UDP connection has to be open for this to work
choose_name(Sock, Timeout) ->
    NameChoice = name_generator(?NODE_NAME_LENGTH),
    namereq_sender(NameChoice, Sock), % Send name request
    %io:format("Sending name: ~p~n", [NameChoice]),
    wait_for_invalid_name(Sock, NameChoice, Timeout).

% Waits for a random amount of time between 2 and 10 seconds, when the time passes it assumes that no one has that name
wait_for_invalid_name(Sock, NameChoice, RemainingTime) ->
    case (RemainingTime =< 0) of
        true -> 
            %io:format("Time's up. Name is accepted. ~p~n", [NameChoice]),
            NameChoice;
        false ->
            Start = erlang:monotonic_time(millisecond), % Starts counting time
            case gen_udp:recv(Sock, 0, RemainingTime) of
                {ok, {_Addr, _Port, Data}} ->
                    case Data of
                        {"INVALID_NAME", _ID} ->
                            %io:format("Name ~p already in use!~n", [NameChoice]),
                            timer:sleep(rand:uniform(8000) + 2000),  % Waits a random time between 2s-10s and retries
                            choose_name(Sock, 10000); 
                        _Other ->
                            Elapsed = erlang:monotonic_time(millisecond) - Start, % Stops counting, waiting for a shorter amount of time
                            NewRemaining = RemainingTime - Elapsed,
                            wait_for_invalid_name(Sock, NameChoice, NewRemaining)
                    end;
                {error, timeout} ->
                    %io:format("Timeout interno: aceptamos nombre ~p~n", [NameChoice]),
                    NameChoice;
                {error, Reason} ->
                    io:format("Error inesperado: ~p. Reintentando...~n", [Reason]),
                    choose_name(Sock, 10000)
            end
    end.

% Randomly generates a name with Sz size, according to the specification details (it can have mayus, minus and digits)
name_generator(Sz) ->
    Alphabet = lists:append([lists:seq(65,90), lists:seq(48,57), lists:seq(97,122)]),
    N = length(Alphabet),
    lists:map(fun(_) -> lists:nth(rand:uniform(N), Alphabet)
        end,
        lists:seq(1, Sz)
    ).

% initializes the UDP socket and concurrently starts the hello sender and udp listener processes.
gen_udp_init() ->
    case gen_udp:open(?UDP_SOCKET, [list, {active, false}, {reuseaddr, true}, {broadcast, true}]) of
        {error, Reason} -> 
            io:format("Error creating UDP socket: [~p] ~n", [Reason]);
        {ok, UDPConnSock} ->
            %io:format("UDP sucessfully created~n"),
            NodeName = choose_name(UDPConnSock, ?NAME_WAIT), % Gets a unique name for the node
            register(?NODE_NAME_HOLDER, spawn(fun() -> node:name_holder_loop(NodeName) end)), % Launches the name holder process
            register(?UDP_SENDER_ID, spawn(fun() -> udp_gen:hello_sender_init(NodeName, UDPConnSock), receive after infinity -> ok end  end)) , % Periodically sends HELLO
            register(?UDP_RECEIVER_ID, spawn(fun() -> udp_discover_listener(UDPConnSock) end)), % Listens for incoming UDP requests
            io:format("Nombre del node: ~p~n", [node:get_node_value()])
    end.


%Periodically sends a HELLO message with the specified format using a UDP broadcast. Requires the TCP socket and the node ID.
hello_sender_init(Id, Sock) ->
    %io:format("Starting hello sender with ID ~p~n", [Id]),
    timer:apply_interval(?HELLO_INTERVAL, ?MODULE, hello_sender, [Id, Sock]).
    
hello_sender(IdStr, Sock) ->
    %io:format("~nSEND HI ~n"),
    Msg = ["HELLO ", IdStr, " ", integer_to_list(?PORT), "\n"],
    gen_udp:send(Sock, ?UDP_BROADCAST, (?UDP_SOCKET), Msg).

% Sends a name request to the broadcast address with the specified ID.
namereq_sender(IdStr, Sock) ->
    Msg = ["NAME_REQUEST ", IdStr, "\n"],
    gen_udp:send(Sock, ?UDP_BROADCAST, (?UDP_SOCKET), Msg).