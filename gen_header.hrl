-define(UDP_SOCKET, 12346).
-define(HELLO_INTERVAL, 17500).
-define(SEARCH_REQUEST_TIMEOUT, 5000).

-define(UDP_SENDER_ID, udp_sender).
-define(UDP_RECEIVER_ID, udp_receiver).

-define(HELLO_UDP, "HELLO").
-define(NAME_REQUEST_UDP, "NAME_REQUEST").
-define(INVALID_UDP, "INVALID").

-record(udpSend, {addr, port, msg}).
-record(fileInfo, {name, size}).
-record(nodeInfo, {id, addr, port}).

-record(collectorElem, {origId, filename, size}).
-define(COLLECTOR_GET, cGet).