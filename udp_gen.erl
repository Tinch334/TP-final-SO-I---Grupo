%Section 4.1, created as general library for common use.
-module(udp_gen).
-export([gen_udp_init/0, gen_udp_sender/1, gen_udp_receiver/1, hello_sender_init/2, hello_sender/2]).

-include("gen_header.hrl").

%There are two nodes, one receives and one sends UDP messages.
gen_udp_init() ->
    case gen_udp:open(?UDP_SOCKET, [list, {active, false}, {reuseaddr, true}]) of
        {ok, Socket} -> register(?UDP_SENDER_ID, spawn(?MODULE, gen_udp_sender, [Socket])),
            register(?UDP_RECEIVER_ID, spawn(?MODULE, gen_udp_receiver, [Socket]));
        {error, Reason} -> io:format("An error occurred creating the UDP gen socket, with error: ~w~n", [Reason])
    end.

gen_udp_sender(Socket) ->
    receive 
        #udpSend{addr = Address, port = Port, msg = Message} -> gen_udp:send(Socket, Address, Port, Message)
    end,
    gen_udp_sender(Socket).


gen_udp_receiver(Socket) ->
    %The longest possible UDP message is less than 30 bytes.
    case gen_udp:recv(Socket, 30) of
        {ok, Data} -> spawn(?MODULE, handle_udp, [Data]);
        {error, Reason} -> io:format("An error occurred receiving from the UDP gen socket, with error: ~w~n", [Reason])
    end,
    gen_udp_receiver(Socket).

handle_udp(Message) ->
    case lists:nth(1, string:tokens(Message, " ")) of
        ?HELLO_UDP -> hello;%ADD NEW NODE.
        ?NAME_REQUEST_UDP -> namereq; %NAME REQUEST.
        ?INVALID_UDP -> invalid%INVALID REQUEST.
    end.

%Periodically sends a HELLO message with the specified format using a UDP broadcast. Requires the TCP socket and the node ID.
hello_sender_init(Id, SocketTCP) ->
    timer:apply_interval(?HELLO_INTERVAL, ?MODULE, hello_sender, [Id, SocketTCP]).
    
hello_sender(Id, SocketTCP) ->
    ?UDP_SENDER_ID ! #udpSend{addr = {255, 255, 255, 255}, port = ?UDP_SOCKET, msg = string:join(["HELLO", Id, SocketTCP, "\n"], " ")}.