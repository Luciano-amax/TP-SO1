-module(p2p_node).
-export([start/1, stop/0]).
-include("config.hrl").

% Inicia el nodo P2P completo
start(TcpPort) ->
    catch unregister(p2p_node),
    catch unregister(file_manager),
    catch unregister(node_registry),
    catch unregister(tcp_server),
    catch unregister(discovery),
    catch unregister(hello_broadcast),
    
    % Se elimina la pausa inicial porque no integra el protocolo del TP
    % y demoraba artificialmente la puesta en marcha del nodo.
    io:format("Leyendo carpeta compartida..."),
    ok = file_manager:start(),
    io:format("OK~n"),
    
    io:format("Iniciando registro de nodos..."),
    ok = node_registry:start(),
    io:format("OK~n"),

    io:format("Obteniendo nombre de Nodo..."),
    {ok, NodeId} = discovery:start(?UDP_PORT, TcpPort),
    io:format("OK (~s)~n", [NodeId]),
    
    io:format("Iniciando servidor TCP..."),
    ok = tcp_server:start(TcpPort),
    io:format("OK~n"),
    
    io:format("Iniciando broadcasts HELLO..."),
    ok = hello_broadcast:start(NodeId, TcpPort),
    io:format("OK~n"),
    
    register(p2p_node, self()),
    
    put(node_id, NodeId),
    put(file_manager, whereis(file_manager)),
    put(node_registry, whereis(node_registry)),
    put(tcp_server, whereis(tcp_server)),
    put(discovery, whereis(discovery)),
    put(hello_broadcast, whereis(hello_broadcast)),
    
    io:format("~nNodo iniciado: ~s (puerto ~p)~n~n", [NodeId, TcpPort]),
    
    % Se omite la pausa previa a la CLI para que el nodo quede operativo
    % ni bien finaliza el consenso y el arranque de sus componentes
    
    % 7. Iniciar CLI (esto bloquea hasta que el usuario escriba "salir")
    cli:start(),
    
    % Cuando CLI termina, limpiamos
    io:format("~nCerrando nodo...~n"),
    stop().

% Detiene el nodo y todos sus componentes
stop() ->
    % Detener todos los procesos hijos
    case whereis(p2p_node) of
        undefined -> 
            io:format("Nodo ya detenido~n"),
            ok;
        Pid ->
            case process_info(Pid, dictionary) of
                {dictionary, Dict} ->
                    case proplists:get_value(tcp_server, Dict) of
                        undefined -> ok;
                        TcpPid -> exit(TcpPid, shutdown)
                    end,
                    case proplists:get_value(file_manager, Dict) of
                        undefined -> ok;
                        FmPid -> exit(FmPid, shutdown)
                    end,
                    case proplists:get_value(node_registry, Dict) of
                        undefined -> ok;
                        NrPid -> exit(NrPid, shutdown)
                    end,
                    case proplists:get_value(discovery, Dict) of
                        undefined -> ok;
                        DiscoveryPid -> exit(DiscoveryPid, shutdown)
                    end,
                    case proplists:get_value(hello_broadcast, Dict) of
                        undefined -> ok;
                        HelloPid -> exit(HelloPid, shutdown)
                    end;
                _ -> ok
            end,
            unregister(p2p_node),
            io:format("Nodo detenido~n"),
            ok
    end.
