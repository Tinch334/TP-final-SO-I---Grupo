%Length of the ID we choose to identify the node
-define(NODE_NAME_LENGTH, 4).

%-------CONNECTION CONFIG-------
-define(PORT, 12345).
-define(UDP_SOCKET, 12346).
-define(HOSTNAME, "localhost").
-define(SHELLNAME, "LAPNP2P-client").
-define(UDP_BROADCAST, {255, 255, 255, 255}).

% Time intervals
-define(DOWLOAD_DATA_TIMEOUT, 10000).
-define(SEARCH_REQUEST_TIMEOUT, 5000).
-define(HELLO_INTERVAL, 17500). % Time between HELLO messages sent by self
-define(NAME_WAIT, 8000). % Time waiting to decide on a name

% Message codes and message configuration
-define(OK_CODE, 101).
-define(NOTFOUND_CODE, 112).
-define(CHUNK_CODE, 111).
-define(MAX_SINGLE_FILE_SIZE, 1024 * 1024 * 4). % Max file size to send as a single message (If it exceeds the threshold we use chunks)

% Used for file_lookup
-record(fileLookupError, {reason}). % If the file_lookup() function has an error, it returns this with the reason of such error
-record(fileLookupSuccess, {files}). % If file_lookup() succeeds, it returns a success along with a list of the fileInfo
-record(fileInfo, {name, size}). % A record for a file name and it's size

% Personal use file paths  
-define(SHARED_PATH, "./compartida").
-define(DOWNLOADS_PATH, "./descargas").
-define(NODE_FILE, "nodes_registry.txt").

% List of available shell commands
-define(SHELL_COMMS, ["EXIT", "NODE_ID", "LIST_FILES", "SEARCH_REQUEST <Filename>", "DOWNLOAD_REQUEST <Filename> <OwnerNodeID>", "LIST_NODES"]).

% Macros used to identify registered processes
-define(NOT_FOUND, notFound). % Not found atomic used when get_info_from_id() doesn't find the ID in the registry
-define(NODE_NAME_HOLDER, name_holder). % The process that keeps track of the node name
-define(UDP_SENDER_ID, udp_sender). % Registered process for sending the periodical hello to other nodes
-define(UDP_RECEIVER_ID, udp_receiver). % Registered process for receiving discovery attempts from other nodes

% Collector specifications
-define(COLLECTOR_GET, collectorGet). % The thread that collects information about a SEARCH_REQUEST
-record(collectorRes, {lst}). % Record used to differentiate a message of the result for the collector
-record(collectorElem, {origId, filename, size}). % An element of the collected information
 
% Record of needed node information
-record(nodeInfo, {ip, id, port}).

