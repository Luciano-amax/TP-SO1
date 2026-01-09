MAKE NUEVO:
# ===========================================
# Makefile para Sistema P2P en Erlang
# Multi-plataforma: Windows y Linux
# ===========================================

# Detectar sistema operativo
ifeq ($(OS),Windows_NT)
    RM = del /Q
    RM_DIR = rmdir /S /Q
    MKDIR = if not exist $(1) mkdir $(1)
    PATH_SEP = \\
    NULL_DEV = nul
else
    RM = rm -f
    RM_DIR = rm -rf
    MKDIR = mkdir -p $(1)
    PATH_SEP = /
    NULL_DEV = /dev/null
endif

.PHONY: all clean run dumpclean test

# Compilar todos los módulos Erlang
all:
	@echo "Compilando modulos Erlang..."
ifeq ($(OS),Windows_NT)
	@if not exist ebin mkdir ebin
	@cd src && for %%f in (*.erl) do @erlc -o ..\ebin %%f
else
	@mkdir -p ebin
	@erlc -o ebin src/*.erl
endif
	@echo "Compilacion completa!"

# Limpiar archivos compilados
clean:
	@echo "Limpiando archivos .beam..."
ifeq ($(OS),Windows_NT)
	@if exist ebin$(PATH_SEP)*.beam del /Q ebin$(PATH_SEP)*.beam
else
	@rm -f ebin/*.beam
endif
	@echo "Limpieza completa!"

# Limpiar archivos dump de crashes
dumpclean:
	@echo "Limpiando archivos .dump..."
ifeq ($(OS),Windows_NT)
	@if exist *.dump del /Q *.dump
	@if exist erl_crash.dump del /Q erl_crash.dump
else
	@rm -f *.dump erl_crash.dump
endif
	@echo "Limpieza de dumps completa!"

# Ejecutar el nodo P2P (requiere compilación previa)
run:
	@echo "Iniciando nodo P2P en puerto 12345..."
	@echo "Usa: p2p_node:start(12345)."
	@erl -pa ebin

# Pruebas rápidas de módulos individuales
test-discovery:
	@echo "Probando discovery..."
	erl -pa ebin -eval "discovery:test()." -s init stop

test-file-manager:
	@echo "Probando file_manager..."
	erl -pa ebin -eval "file_manager:test()." -s init stop

test-node-registry:
	@echo "Probando node_registry..."
	erl -pa ebin -eval "node_registry:test()." -s init stop

# Ayuda
help:
	@echo "Comandos disponibles:"
	@echo "  make all        - Compila todos los modulos"
	@echo "  make clean      - Borra archivos .beam"
	@echo "  make dumpclean  - Borra archivos .dump"
	@echo "  make run        - Inicia el shell de Erlang con el proyecto"
	@echo "  make help       - Muestra esta ayuda"