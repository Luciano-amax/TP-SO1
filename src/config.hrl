%%%-------------------------------------------------------------------
%%% Definición de Macros utilizadas para el proyecto
%%%-------------------------------------------------------------------

%% Puertos
-define(UDP_PORT, 12346).
-define(TCP_PORT_DEFAULT, 12345).

%% Directorios
-define(SHARED_DIR, "./compartida").
-define(DOWNLOAD_DIR, "./descargas").

%% Timeouts y intervalos
-define(ID_REQUEST_TIMEOUT, 3000).
-define(HELLO_INTERVAL_MIN, 5000).
-define(HELLO_INTERVAL_MAX, 8000).
-define(NODE_TIMEOUT, 45000).
-define(NODE_TIMEOUT_SECONDS, 45).
-define(CLEANUP_INTERVAL, 15000).

-define(SEARCH_TIMEOUT, 5000).
-define(DOWNLOAD_TIMEOUT, 180000).
-define(CHUNK_TIMEOUT, 15000).
-define(TCP_CONNECT_TIMEOUT, 8000).
-define(TCP_HEADER_TIMEOUT, 8000).
-define(PROCESS_REPLY_TIMEOUT, 5000).

%% Protocolo
-define(CHUNK_SIZE, 4194304).
-define(LARGE_FILE_THRESHOLD, 4194304).
-define(WORKERS_PER_NODE, 2).

%% Códigos de respuesta TCP
-define(CODE_OK, 101).
-define(CODE_CHUNK, 111).
-define(CODE_NOTFOUND, 112).

%% Direcciones de broadcast
-define(BROADCAST_ADDR, {255, 255, 255, 255}).
-define(BROADCAST_OPEN, {0, 0, 0, 0}).

%% Salida por consola
-define(SHOW_DISCOVERY_LOGS, false).

%% Generación de IDs
-define(NODE_ID_CHARS, "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789").
-define(NODE_ID_LENGTH, 4).
