===========================================
# SISTEMA P2P DESCENTRALIZADO EN ERLANG
===========================================

## Descripción general

Sistema P2P descentralizado desarrollado en Erlang para compartir y descargar archivos dentro de una LAN. Cada nodo actúa simultáneamente como cliente y servidor, utilizando:

- **UDP**: Descubrimiento de nodos (broadcasts HELLO) y negociación de IDs únicos
- **TCP**: Transferencia de archivos y búsqueda de archivos
- **Descargas multi-fuente**: Descarga de archivos desde múltiples nodos en paralelo, con soporte para descarga por chunks

El sistema está compuesto por módulos especializados que manejan: descubrimiento de nodos, registro de nodos, gestión de archivos, transferencias TCP, descargas paralelas con chunks, y una interfaz CLI interactiva.


## Cómo ejecutar el programa

### 1. Compilación

```bash
make all
```

Esto compilará todos los archivos `.erl` en el directorio `src/` generando los archivos `.beam` en `ebin/`.

### 2. Preparación de directorios

Antes de ejecutar, asegurarse de que existan las carpetas requeridas:

```bash
mkdir -p compartida
mkdir -p descargas/chunks
```

- **compartida/**: Directorio donde se guardan los archivos que deseas compartir con otros nodos
- **descargas/**: Directorio donde se guardarán los archivos descargados
- **descargas/chunks/**: Directorio donde se almacenan los chunks durante descargas multi-fuente

### 3. Configuración

Edita el archivo `src/config.hrl` para ajustar parámetros según tu red:

```erlang
%% Puertos
-define(UDP_PORT, 12346).              % Puerto para broadcasts HELLO
-define(TCP_PORT_DEFAULT, 12345).      % Puerto TCP del nodo

%% Directorios
-define(SHARED_DIR, "./compartida").    % Carpeta de archivos compartidos
-define(DOWNLOAD_DIR, "./descargas").   % Carpeta de descargas

%% Timeouts (en milisegundos)
-define(HELLO_INTERVAL_MIN, 15000).    % 15 segundos
-define(HELLO_INTERVAL_MAX, 20000).    % 20 segundos
-define(NODE_TIMEOUT, 45000).          % Inactividad máxima de nodo
-define(DOWNLOAD_TIMEOUT, 300000).     % Timeout para descargas

%% Protocolo
-define(CHUNK_SIZE, 4194304).          % Tamaño de chunk: 4MB
-define(LARGE_FILE_THRESHOLD, 4194304).% Umbral para descarga en chunks

%% Broadcast (ajusta según tu red)
-define(BROADCAST_ADDR, {25, 255, 255, 255}).  % Para Hamachi
-define(BROADCAST_ADDR, {192, 168, 1, 255}).  % Para red local 192.168.1.X
-define(BROADCAST_ADDR, {255, 255, 255, 255}). % Para LAN general
```

**Parámetros configurables:**
- `UDP_PORT`: Puerto UDP para broadcasts (debe ser el mismo en todos los nodos)
- `TCP_PORT_DEFAULT`: Puerto TCP de este nodo (puede variar por nodo)
- `SHARED_DIR`: Ruta a la carpeta de archivos compartidos
- `DOWNLOAD_DIR`: Ruta a la carpeta de descargas
- `HELLO_INTERVAL_MIN/MAX`: Intervalo aleatorio de broadcast HELLO
- `NODE_TIMEOUT`: Tiempo máximo de inactividad antes de considerar nodo inactivo
- `CHUNK_SIZE`: Tamaño de cada chunk para descargas paralelas
- `BROADCAST_ADDR`: Dirección de broadcast (varía según tipo de red)

### 4. Ejecución

Usa el Makefile para compilar y ejecutar el proyecto:

```bash
make run
```

Esto compilará todos los módulos automáticamente y abrirá la consola de Erlang con el proyecto cargado. Dentro de la consola, ejecuta:

```erlang
p2p_node:start(12345).   % Inicia el nodo en puerto TCP 12345
```

Una vez iniciado, aparecerá el prompt `p2p> ` donde puedes ingresar comandos.


## Comandos CLI

### Comandos de Información

#### `ayuda`
Muestra la lista de comandos disponibles.

```
p2p> ayuda

Comandos disponibles:
  id_nodo              - Muestra el ID unico del nodo
  listar_mis_archivos  - Lista los archivos compartidos
  getNodes             - Lista los nodos conocidos en la red
  buscar <patron>      - Busca archivos en la red
  descargar <archivo>  - Descarga desde multiples nodos
  descargar <archivo> <nodo> - Descarga de un nodo especifico
  salir                - Cierra el nodo P2P
  ayuda                - Muestra esta ayuda
```

#### `id_nodo`
Muestra el ID único del nodo actual en la red. Este ID se negocia automáticamente mediante consenso distribuido en el inicio.

```
p2p> id_nodo
NodoID: aB3x
```

#### `listar_mis_archivos`
Lista todos los archivos que el nodo actual está compartiendo (ubicados en la carpeta `compartida/`). Muestra el nombre y tamaño en MB de cada archivo.

```
p2p> listar_mis_archivos

Archivos compartidos:
  documento.pdf (2.50 MB)
  video.mp4 (150.75 MB)
  imagen.jpg (5.20 MB)
```

#### `getNodes`
Muestra la lista de todos los nodos conocidos en la red, incluyendo su ID, dirección IP y puerto TCP. Se actualiza automáticamente a través de broadcasts HELLO.

```
p2p> getNodes

Nodos conocidos:
  aB3x (192.168.1.100:12345)
  kXy2 (192.168.1.101:12346)
  pQ9w (192.168.1.102:12345)
```

### Comandos de Búsqueda

#### `buscar <patrón>`
Busca archivos en todos los nodos de la red que coincidan con el patrón especificado.
Soporta wildcards (`*` para cualquier secuencia, `?` para un carácter).

Sintaxis:
```
p2p> buscar <patrón>
```

Ejemplos:
```
p2p> buscar *.pdf
Búsqueda completa. Resultados:
  Nodo aB3x: documento.pdf (2500000 bytes)
  Nodo kXy2: reporte.pdf (1800000 bytes)
```

```
p2p> buscar video*
Búsqueda completa. Resultados:
  Nodo pQ9w: video.mp4 (150750000 bytes)
  Nodo aB3x: video.avi (120000000 bytes)
```

### Comandos de Descarga

#### `descargar <archivo>`
Inicia una descarga del archivo especificado desde múltiples nodos en paralelo. El sistema busca automáticamente el archivo y elige la mejor estrategia:

- **Archivos pequeños (≤ 4MB)**: Se descargan completos desde un nodo
- **Archivos grandes (> 4MB)**: Se dividen en chunks de 4MB y se descargan en paralelo desde múltiples nodos simultáneamente (4 workers por nodo)

Los chunks se almacenan temporalmente en `descargas/chunks/`, y cuando termina la descarga, se ensamblan en el archivo final en `descargas/`.

Sintaxis:
```
p2p> descargar <archivo>
```

Ejemplo:
```
p2p> descargar documento.pdf
Buscando nodos con documento.pdf...
Iniciando descarga paralela (2 nodos, 8 workers)
Descargando chunk 0 de nodo aB3x
Descargando chunk 1 de nodo kXy2
Descargando chunk 2 de nodo aB3x
...
Archivo ensamblado: documento.pdf
Descarga completada
```

#### `descargar <archivo> <nodo>`
Descarga un archivo específico desde un nodo particular (sin usar descarga multi-fuente).
Útil cuando quieres especificar exactamente de dónde descargar o si necesitas una descarga simple.

Sintaxis:
```
p2p> descargar <archivo> <nodo>
```

Ejemplo:
```
p2p> descargar documento.pdf aB3x
Descargando...
Archivo guardado en: descargas/documento.pdf
```

### Comando de Salida

#### `salir`
Detiene el nodo, cierra todas las conexiones y finaliza el proceso. Limpia correctamente todos los procesos asociados.

```
p2p> salir
Cerrando nodo...
Nodo detenido
```

## Makefile - Comandos disponibles

```bash
make all          # Compila todos los archivos .erl en src/ generando .beam en ebin/
make run          # Compila y ejecuta Erlang automáticamente 
make clean        # Elimina todos los archivos .beam compilados
make dumpclean    # Elimina los archivos .dump y erl_crash.dump
make help         # Muestra la lista de comandos disponibles
```

El Makefile es multi-plataforma y funciona tanto en Windows como en Linux/Mac.

### Ejemplo Inicio del nodo

```bash
$ make run
1> p2p_node:start(12345).
Leyendo carpeta compartida...OK
Obteniendo nombre de Nodo...OK (xK9m)
Iniciando registro de nodos...OK
Iniciando servidor TCP...OK
Iniciando broadcasts HELLO...OK

Nodo iniciado: xK9m (puerto 12345)

p2p>
```

### Sincronización de nodos
- Los nodos tardan 15-20 segundos en descubrirse mutuamente
- Los broadcasts HELLO se envían cada 15-20 segundos de forma aleatoria
- Un nodo se considera inactivo si no envía HELLO en 45 segundos

### Descargas paralelas
- Cada nodo origen genera 4 workers paralelos
- El tamaño de chunk es de 4MB por defecto
- Los chunks se descargan simultáneamente desde múltiples nodos

