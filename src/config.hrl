%%%-------------------------------------------------------------------
%%% Definición de Macros utilizadas para el proyecto (revisar informe)
%%%-------------------------------------------------------------------

%% Puertos
-define(UDP_PORT, 12346).
-define(TCP_PORT_DEFAULT, 12345).

%% Directorios (se configuran dinamicamente por puerto)
%% Los valores por defecto se usan si no se configura
-define(SHARED_DIR, "./compartida").
-define(DOWNLOAD_DIR, "./descargas").

%% Timeouts y intervalos
% Se fija un tiempo moderado para el consenso de IDs:
% permite detectar colisiones sin demorar en exceso el arranque.
-define(ID_REQUEST_TIMEOUT, 5000).
% Se restablece un intervalo periódico más estable para HELLO,
% adecuado para una LAN y consistente con una operación normal del nodo.
-define(HELLO_INTERVAL_MIN, 15000).
-define(HELLO_INTERVAL_MAX, 20000).
% Se vuelve a un timeout de inactividad conservador para evitar
% falsos positivos al depurar nodos o ante pequeñas demoras de red.
-define(NODE_TIMEOUT, 45000).
-define(NODE_TIMEOUT_SECONDS, 45).
% La limpieza periódica acompaña el timeout de inactividad y evita
% borrar nodos válidos por chequeos demasiado agresivos.
-define(CLEANUP_INTERVAL, 30000).

-define(SEARCH_TIMEOUT, 10000).         % 10 segundos para busqueda
-define(DOWNLOAD_TIMEOUT, 300000).       % 120 segundos para descarga
-define(CHUNK_TIMEOUT, 30000).          % 30 segundos por chunk

%% Protocolo
-define(CHUNK_SIZE, 4194304).           % 4MB = 4*1024*1024 bytes
-define(LARGE_FILE_THRESHOLD, 4194304). % 4MB = 4*1024*1024 bytes
-define(WORKERS_PER_NODE, 1).           % Workers paralelos por nodo

%% Códigos de respuesta TCP
-define(CODE_OK, 101).
-define(CODE_CHUNK, 111).
-define(CODE_NOTFOUND, 112).

%% Direcciones de broadcast
%% Para Hamachi usar {25, 255, 255, 255} 
%% Para red local 192.168.1.X usar {192, 168, 1, 255}
%% Para LAN general usar {255, 255, 255, 255}
-define(BROADCAST_ADDR, {255, 255, 255, 255}).
-define(BROADCAST_OPEN, {0, 0, 0, 0}).

%% Generación de IDs
-define(NODE_ID_CHARS, "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789").
-define(NODE_ID_LENGTH, 4).     %%
