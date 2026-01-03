% Interfaz de línea de comandos para interactuar con el nodo
-module(cli).
-export([start/0]).

% Arranca la CLI y muestra el menú
start() ->
    io:format("~n=== Sistema P2P ===~n"),
    print_help(),
    command_loop().

% Loop principal que lee y procesa comandos
command_loop() ->
    Line = io:get_line("p2p> "),
    case Line of
        eof ->
            io:format("~nSaliendo...~n"),
            ok;
        _ ->
            Command = string:trim(Line),
            process_command(Command),
            command_loop()
    end.

% Procesa cada comando ingresado
process_command("") ->
    ok;

process_command("ayuda") ->
    print_help();

% §3.1.5: Comando id_nodo - Muestra el ID del nodo actual
process_command("id_nodo") ->
    % Obtenemos el ID desde el proceso p2p_node
    NodeId = case whereis(p2p_node) of
        undefined -> 
            "desconocido";
        Pid -> 
            case process_info(Pid, dictionary) of
                {dictionary, Dict} ->
                    proplists:get_value(node_id, Dict, "desconocido");
                _ -> "desconocido"
            end
    end,
    io:format("~nID del nodo: ~s~n", [NodeId]);

% §3.1.5: Comando listar_mis_archivos - Lista archivos de la carpeta compartida
process_command("listar_mis_archivos") ->
    Files = file_manager:get_shared_files(),
    io:format("~n=== Mis Archivos Compartidos ===~n"),
    case Files of
        [] ->
            io:format("No hay archivos compartidos~n");
        _ ->
            % Mostramos cada archivo con su tamaño en MB
            lists:foreach(fun({Name, Size, _Path}) ->
                SizeMB = Size / 1048576,
                io:format("  • ~s (~.2f MB)~n", [Name, SizeMB])
            end, Files),
            io:format("~nTotal: ~p archivo(s)~n", [length(Files)])
    end;

% Comando buscar - Busca archivos en todos los nodos de la red
% quiza es demasiado rustico (revisar forma de flexibilizacion)
process_command("buscar " ++ Pattern) ->
    search:search_all_nodes(Pattern);

% §3.1.5: Comando salir - Cierra el nodo
process_command("salir") ->
    io:format("~nCerrando el nodo...~n"),
    % Detenemos todos los componentes
    discovery:stop(),
    tcp_server:stop(),
    file_manager:stop(),
    node_registry:stop(),
    erlang:halt(0);

% Comando desconocido
process_command(Unknown) ->
    io:format("Comando desconocido: ~s~n", [Unknown]),
    io:format("Escribí 'ayuda' para ver los comandos disponibles~n").

% Muestra la lista de comandos disponibles
print_help() ->
    io:format("~nComandos disponibles:~n"),
    io:format("  id_nodo              - Muestra el ID único del nodo~n"),
    io:format("  listar_mis_archivos  - Lista los archivos compartidos~n"),
    io:format("  buscar <patron>      - Busca archivos en la red~n"),
    io:format("  salir                - Cierra el nodo P2P~n"),
    io:format("  ayuda                - Muestra esta ayuda~n~n").
