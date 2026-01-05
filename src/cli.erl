% Interfaz de linea de comandos
-module(cli).
-export([start/0]).

start() ->
    io:format("~n=== Sistema P2P ===~n"),
    print_help(),
    command_loop().

% Loop principal de lectura de comandos
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

% Comando nodos - Lista nodos conocidos en la red
process_command("getNodes") ->
    Nodes = node_registry:get_all_nodes(),
    io:format("~n=== Nodos Conocidos ===~n"),
    case Nodes of
        [] ->
            io:format("No hay nodos conocidos~n");
        _ ->
            lists:foreach(fun({NodeId, Ip, Port}) ->
                io:format("  • ~s (~p:~w)~n", [NodeId, Ip, Port])
            end, Nodes),
            io:format("~nTotal: ~p nodo(s)~n", [length(Nodes)])
    end;

% Comando buscar - Busca archivos en todos los nodos de la red
% quiza es demasiado rustico (revisar forma de flexibilizacion)
process_command("buscar " ++ Pattern) ->
    search:search_all_nodes(Pattern);

% Comando descargar - Descarga un archivo desde un nodo específico (concurrente)
process_command("descargar " ++ Rest) ->
    Parts = string:tokens(Rest, " "),
    case length(Parts) of
        2 ->
            FileName = lists:nth(1, Parts),
            NodeId = lists:nth(2, Parts),
            spawn(fun() -> download:download_from_node(FileName, NodeId) end),
            io:format("Descarga iniciada en segundo plano...~n");
        _ ->
            io:format("Uso: descargar <nombre_archivo> <nodo_id>~n")
    end;

% Comando salir - Cierra el Nodo
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
    io:format("  getNodes                - Lista los nodos conocidos en la red~n"),
    io:format("  buscar <patron>      - Busca archivos en la red~n"),
    io:format("  descargar <archivo> <nodo> - Descarga un archivo de un nodo~n"),
    io:format("  salir                - Cierra el nodo P2P~n"),
    io:format("  ayuda                - Muestra esta ayuda~n~n").
