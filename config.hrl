%% https://www.erlang.org/doc/system/macros.html
-define(PORT, 12345).
-define(OK_CODE, 101).
-define(NOTFOUND_CODE, 112).
-define(CHUNK_CODE, 111).
-define(SHARED_PATH, "./compartida").
-define(DOWNLOADS_PATH, "./descargas").
-define(REG_PATH, "nodes_registry.txt").
-define(HOSTNAME, "localhost").
-define(SHELLNAME, "LAPNP2P-client").
-define(SHELL_COMMS, ["id_nodo", "listar_mis_archivos", "help", "salir", "DOWNLOAD_REQUEST <FileName> <OwnerNodeID>"]).
-define(TEST_NAMES, ["cuento.txt", "sdfsdf.txt", "crimen.txt", "potente.txt"]).
-define(MAX_SINGLE_FILE_SIZE, 1024 * 4). % ajustar a 4MB
-define(NODE_NAME_LENGTH, 4).

