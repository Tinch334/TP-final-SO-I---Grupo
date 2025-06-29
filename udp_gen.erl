%Section 4.1, created as general library for common use.
-module(udp_gen).
-export([gen_udp_init/0, hello_sender_init/2, hello_sender/2, namereq_sender/2, udp_discover_listener/1, handle_udp_req/3, choose_name/1]).

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
            handle_udp_req(Str, Ip, Sock);
        {error, Reason} ->
            io:format("Error al recibir UDP: ~p~n", [Reason]),
            udp_discover_listener(Sock)
    end.

% handle the request received from UDP socket
handle_udp_req(Str, IpFrom, Sock) ->
    case string:tokens(Str, " \n") of
        ["NAME_REQUEST", IdStr] ->
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
            io:format("add ~p ~p ~p to register~n", [IpFrom, IdStr, PortStr]),
            utils:add_node_to_registry(IpFrom, IdStr, PortStr),
            udp_discover_listener(Sock);

        _ ->
            io:format("Mensaje desconocido: ~p~n", [Str]),
            udp_discover_listener(Sock)
    end.

% follows the protocol to choose a unique name for the node. precond: an active UDP connection is open
choose_name(Sock) ->
    NameChoice = nodo:name_generator(?NODE_NAME_LENGTH),  % randomly generate a name
    namereq_sender(self(), NameChoice),
    io:format("Sending name: ~p~n", [NameChoice]),
    case gen_udp:recv(Sock, 0, 10000) of
        % do something with InvalidatedID
        {"INVALID_NAME", _InvalidatedID} ->
            io:format("Name ~p already in use! ~n", [NameChoice]), 
            timer:sleep(lists:nth(1, lists:seq(2000, 10000))),  %% wait a random time and retry
            choose_name(Sock);
        {error, timeout} ->
            io:format("No reply was received, so node name may be valid~n"),
            NameChoice;
        {error, Reason} ->
            io:format("An unknown error occurred: ~w~nChoosing a new name~n", [Reason]),
            choose_name(Sock)
    end.

% initializes the UDP socket and concurrently starts the hello sender and udp listener processes.
gen_udp_init() ->
    case gen_udp:open(?UDP_SOCKET, [list, {active, false}, {reuseaddr, true}]) of
        {error, Reason} -> 
            io:format("Error creating UDP socket: [~p] ~n", [Reason]);
        {ok, UDPConnSock} ->
            io:format("UDP sucessfully created~n"),
            NodeName = choose_name(UDPConnSock), % get unique name for the node
            register(name_holder, spawn(fun() -> nodo:name_holder_loop(NodeName) end)), % launch the name holder process
            register(id_holder, spawn(fun() -> id_holder_loop(dict:new()) end)),
            register(?UDP_SENDER_ID, spawn(fun() -> udp_gen:hello_sender_init(self(), NodeName), receive after infinity -> ok end  end)) , % periodically send HELLO
            register(?UDP_RECEIVER_ID, spawn(fun() -> udp_discover_listener(UDPConnSock) end)), % listen for incoming UDP requests
            io:format("Mi nombre es: ~p~n", [nodo:get_node_value()])
    end.


%Periodically sends a HELLO message with the specified format using a UDP broadcast. Requires the TCP socket and the node ID.
hello_sender_init(Sender, Id) ->
    io:format("Starting hello sender with ID ~p~n", [Id]),
    timer:apply_interval(?HELLO_INTERVAL, ?MODULE, hello_sender, [Sender, Id]).
    
hello_sender(Sender, IdStr) ->
    %io:format("TEST ~n"),
    Sender ! #udpSend{addr = {255, 255, 255, 255}, port = ?UDP_SOCKET, msg = string:join(["HELLO", IdStr, integer_to_list(?PORT),  "\n"], " ")}.

% Sends a name request to the broadcast address with the specified ID.
namereq_sender(Sender, IdStr) ->
    Sender ! #udpSend{addr = {255, 255, 255, 255}, port = ?UDP_SOCKET, msg = string:join(["NAME_REQUEST", IdStr, "\n"], " ")}.
