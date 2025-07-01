%TODO: Separate macros by section and explain what they do.
-define(PORT, 12345).
-define(OK_CODE, 101).
-define(NOTFOUND_CODE, 112).
-define(CHUNK_CODE, 111).

-define(SHARED_PATH, "./compartida").
-define(DOWNLOADS_PATH, "./descargas").
-define(REG_PATH, "nodes_registry.txt").

-define(HOSTNAME, "localhost").
-define(SHELLNAME, "LAPNP2P-client").
-define(SHELL_COMMS, ["EXIT", "NODE_ID", "LIST_FILES", "SEARCH_REQUEST <Filename>", "DOWNLOAD_REQUEST <Filename> <OwnerNodeID>", "LIST_NODES"]).
-define(TEST_NAMES, ["cuento.txt", "sdfsdf.txt", "crimen.txt", "potente.txt"]).
-define(MAX_SINGLE_FILE_SIZE, 1024 * 1024 * 4).
-define(NODE_NAME_LENGTH, 4).

-define(DOWLOAD_DATA_TIMEOUT, 10000).

-define(NAME_WAIT, 8000).

-define(SEARCH_REQUEST_TIMEOUT, 5000).
-define(COLLECTOR_GET, collectorGet).

-define(NODE_FILE, "nodes_registry.txt").

-record(collectorElem, {origId, filename, size}).

-record(fileLookupError, {reason}).
-record(fileLookupSuccess, {files}).
-record(fileInfo, {name, size}).

-record(nodeInfo, {ip, id, port}).

-define(UDP_SOCKET, 12346).
-define(HELLO_INTERVAL, 17500).

-define(UDP_SENDER_ID, udp_sender).
-define(UDP_RECEIVER_ID, udp_receiver).

-define(HELLO_UDP, "HELLO").
-define(NAME_REQUEST_UDP, "NAME_REQUEST").
-define(INVALID_UDP, "INVALID").

-record(udpSend, {addr, port, msg}).

-define(NOT_FOUND, notFound).

-define(NODE_NAME_HOLDER, name_holder).

-record(collectorRes, {lst}).

-define(UDP_BROADCAST, {255, 255, 255, 255}).