% Interfaz de linea de comandos
-module(cli).
-export([start/0]).

% Arranca la interfaz y muestra los comandos disponibles.
start() ->
    print_help(),
    command_loop().

% Mantiene el loop principal de lectura de comandos.
command_loop() ->
    Line = io:get_line("p2p> "),
    case Line of
        eof ->
            io:format("~nSaliendo...~n"),
            notify_cli_closed(),
            ok;
        _ ->
            Command = string:trim(Line),
            process_command(Command),
            command_loop()
    end.

% Ignora lineas vacias para no ensuciar la salida.
process_command("") ->
    ok;

% Vuelve a mostrar la ayuda desde la CLI.
process_command("ayuda") ->
    print_help();

% Consulta el ID guardado en el coordinador del nodo.
process_command("id_nodo") ->
    NodeId = case whereis(p2p_node) of
        undefined -> 
            "desconocido";
        Pid -> 
            % El coordinador guarda el estado en su diccionario de proceso.
            case process_info(Pid, dictionary) of
                {dictionary, Dict} ->
                    proplists:get_value(node_id, Dict, "desconocido");
                _ -> "desconocido"
            end
    end,
    io:format("NodoID: ~s~n", [NodeId]);

% Muestra la lista local de archivos compartidos.
process_command("listar_mis_archivos") ->
    Files = file_manager:get_shared_files(),
    io:format("~nArchivos compartidos:~n"),
    case Files of
        [] ->
            io:format("  (sin archivos)~n");
        _ ->
            lists:foreach(fun({Name, Size, _Path}) ->
                SizeMB = Size / 1048576,
                io:format("  ~s (~.2f MB)~n", [Name, SizeMB])
            end, Files)
    end;

% Imprime los nodos conocidos por el registro local.
process_command("getNodes") ->
    Nodes = node_registry:get_all_nodes(),
    io:format("~nNodos conocidos:~n"),
    case Nodes of
        [] ->
            io:format("  (sin nodos)~n");
        _ ->
            lists:foreach(fun({NodeId, Ip, Port}) ->
                io:format("  ~s (~p:~w)~n", [NodeId, Ip, Port])
            end, Nodes)
    end;

% Dispara una busqueda distribuida por patron.
process_command("buscar " ++ Pattern) ->
    search:search_all_nodes(Pattern);

% Interpreta la descarga simple o multi-fuente segun los parametros.
process_command("descargar " ++ Rest) ->
    Parts = string:tokens(Rest, " "),
    case length(Parts) of
        1 ->
            FileName = lists:nth(1, Parts),
            % La descarga corre aparte para no clavar la CLI.
            spawn(fun() -> download:download_multi_source(FileName) end),
            ok;
        2 ->
            FileName = lists:nth(1, Parts),
            NodeId = lists:nth(2, Parts),
            spawn(fun() -> download:download_from_node(FileName, NodeId) end),
            io:format("Descargando...~n");
        _ ->
            io:format("Uso: descargar <archivo> [nodo]~n")
    end;

% Pide el cierre ordenado del nodo y termina la CLI.
process_command("salir") ->
    io:format("Cerrando...~n"),
    p2p_node:stop(),
    notify_cli_closed(),
    exit(normal);

% Comando desconocido
process_command(Unknown) ->
    io:format("Comando desconocido: ~s~n", [Unknown]),
    io:format("Escribí 'ayuda' para ver los comandos disponibles~n").

% Muestra la lista de comandos disponibles
print_help() ->
    io:format("~nComandos disponibles:~n"),
    io:format("  id_nodo              - Muestra el ID unico del nodo~n"),
    io:format("  listar_mis_archivos  - Lista los archivos compartidos~n"),
    io:format("  getNodes             - Lista los nodos conocidos en la red~n"),
    io:format("  buscar <patron>      - Busca archivos en la red~n"),
    io:format("  descargar <archivo>  - Descarga desde multiples nodos~n"),
    io:format("  descargar <archivo> <nodo> - Descarga de un nodo especifico~n"),
    io:format("  salir                - Cierra el nodo P2P~n"),
    io:format("  ayuda                - Muestra esta ayuda~n~n").

% Le avisa al coordinador que este proceso de CLI ya termino.
notify_cli_closed() ->
    case whereis(p2p_node) of
        undefined ->
            ok;
        Pid ->
            Pid ! {cli_stopped, self()}
    end.
