%%%-------------------------------------------------------------------
%%% Definición de Macros utilizadas para el proyecto (revisar informe)
%%%-------------------------------------------------------------------

%% Puertos
-define(UDP_PORT, 12346).
-define(TCP_PORT_DEFAULT, 12345).

%% Directorios (agregar luego archivos ejemplo)
-define(SHARED_DIR, "./compartida").
-define(DOWNLOAD_DIR, "./descargas").

%% Timeouts y intervalos
-define(ID_REQUEST_TIMEOUT, 10000).
-define(HELLO_INTERVAL_MIN, 15000).     % 15 segundos
-define(HELLO_INTERVAL_MAX, 20000).     % 20 segundos
-define(NODE_TIMEOUT, 45000).           % 45 segundos sin HELLO = inactivo
-define(NODE_TIMEOUT_SECONDS, 45).
-define(CLEANUP_INTERVAL, 30000).       % 30 segundos entre limpiezas
-define(SEARCH_TIMEOUT, 10000).         % 10 segundos para busqueda
-define(DOWNLOAD_TIMEOUT, 30000).       % 30 segundos para descarga
-define(CHUNK_TIMEOUT, 30000).          % 30 segundos por chunk

%% Protocolo
-define(CHUNK_SIZE, 1048576).           % 1MB = 1048576 bytes
-define(LARGE_FILE_THRESHOLD, 4194304). % 4MB = 4*1024*1024 bytes

%% Códigos de respuesta TCP
-define(CODE_OK, 101).
-define(CODE_CHUNK, 111).
-define(CODE_NOTFOUND, 112).

%% Direcciones de broadcast
-define(BROADCAST_ADDR, {255, 255, 255, 255}).
-define(BROADCAST_OPEN, {0, 0, 0, 0}).

%% Generación de IDs
-define(NODE_ID_CHARS, "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789").
-define(NODE_ID_LENGTH, 4).     %%
