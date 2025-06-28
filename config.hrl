%% https://www.erlang.org/doc/system/macros.html
-define(SHARED_PATH, "./compartida").
-define(DOWNLOADS_PATH, "./descargas").
-define(HOSTNAME, "localhost").
-define(SHELLNAME, "LAPNP2P-client").
-define(SHELL_COMMS, ["id_nodo", "listar_mis_archivos", "help", "salir", "DOWNLOAD_REQUEST <FileName> <OwnerNodeID>"]).
-define(TEST_NAMES, ["cuento.txt", "sdfsdf.txt", "crimen.txt", "potente.txt"]).

-record(fileLookupError, {reason}).
-record(fileLookupSuccess, {files}).
-record(fileInfo, {name, size}).
