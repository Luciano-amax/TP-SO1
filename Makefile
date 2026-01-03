# ===========================================
# Makefile para Sistema P2P en Erlang
# =========================================

.PHONY: all clean run dumpclean test

# Compilar todos los módulos Erlang
all:
	@echo "Compilando modulos Erlang..."
	erlc -o ebin src/*.erl
	@echo "Compilacion completa!"

# Limpiar archivos compilados
clean:
	@echo "Limpiando archivos .beam..."
	rm -f ebin/*.beam
	@echo "Limpieza completa!"

# Limpiar archivos dump de crashes
dumpclean:
	@echo "Limpiando archivos .dump..."
	rm -f *.dump erl_crash.dump
	@echo "Limpieza de dumps completa!"

# Ejecutar el nodo P2P (requiere compilación previa)
run:
	@echo "Iniciando nodo P2P en puerto 12345..."
	@echo "Usa: p2p_node:start(12345)."
	erl -pa ebin

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
