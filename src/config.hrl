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
-define(HELLO_INTERVAL, 15000).
-define(NODE_TIMEOUT, 40000).
-define(NODE_TIMEOUT_SECONDS, 40).      % 40 segundos sin HELLO = inactivo
-define(SEARCH_TIMEOUT, 10000).         % 10 segundos para búsqueda
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
