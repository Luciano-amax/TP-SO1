% Interfaz de linea de comandos
-module(cli).
-export([start/0]).

start() ->
    print_help(),
    command_loop().

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

process_command("") ->
    ok;

process_command("ayuda") ->
    print_help();

process_command("id_nodo") ->
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
    io:format("NodoID: ~s~n", [NodeId]);

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

process_command("buscar " ++ Pattern) ->
    search:search_all_nodes(Pattern);

process_command("descargar " ++ Rest) ->
    Parts = string:tokens(Rest, " "),
    case length(Parts) of
        2 ->
            FileName = lists:nth(1, Parts),
            NodeId = lists:nth(2, Parts),
            spawn(fun() -> download:download_from_node(FileName, NodeId) end),
            io:format("Descargando...~n");
        _ ->
            io:format("Uso: descargar <archivo> <nodo>~n")
    end;

process_command("salir") ->
    io:format("Cerrando...~n"),
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
