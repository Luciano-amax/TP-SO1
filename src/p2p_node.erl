-module(p2p_node).
-export([start/1, stop/0]).
-include("config.hrl").

% Módulo coordinador principal del nodo P2P
% Inicializa todos los componentes y lanza la CLI
start(TcpPort) ->
    % Limpiamos cualquier registro previo de todos los procesos
    catch unregister(p2p_node),
    catch unregister(file_manager),
    catch unregister(node_registry),
    catch unregister(tcp_server),
    catch unregister(discovery),
    
    timer:sleep(1000),
    
    io:format("~n=== Iniciando nodo P2P ===~n~n"),
    
    % 1. Escanear archivos compartidos
    io:format("Leyendo carpeta compartida..."),
    FileManagerPid = file_manager:start(),
    io:format("Completo~n"),
    
    % 2. Consenso de ID
    io:format("Obteniendo nombre de Nodo..."),
    {ok, NodeId} = discovery:request_node_id(),
    io:format("NodoID confirmado: ~s~n", [NodeId]),
    
    % 3. Iniciar registro de nodos
    io:format("Creando registro de nodos..."),
    NodeRegistryPid = node_registry:start(),
    io:format("Completo~n"),
    
    % 4. Iniciar servidor TCP
    io:format("Iniciando servidor TCP..."),
    TcpServerPid = tcp_server:start(TcpPort),
    io:format("Completo~n"),
    
    % 5. Registrar este proceso para que CLI pueda consultarnos
    register(p2p_node, self()),
    
    % 6. Guardar NodeId en el diccionario del proceso para que CLI lo consulte
    put(node_id, NodeId),
    put(file_manager, FileManagerPid),
    put(node_registry, NodeRegistryPid),
    put(tcp_server, TcpServerPid),
    
    io:format("~n=== Nodo P2P iniciado ===~n"),
    io:format("NodoID: ~s~n", [NodeId]),
    io:format("Puerto TCP: ~p~n~n", [TcpPort]),
    
    timer:sleep(1000),
    
    % 7. Iniciar CLI (esto bloquea hasta que el usuario escriba "salir")
    cli:start(),
    
    % Cuando CLI termina, limpiamos
    io:format("~nCerrando nodo...~n"),
    stop().

stop() ->
    % Detener todos los procesos hijos
    case whereis(p2p_node) of
        undefined -> 
            io:format("Nodo ya detenido~n"),
            ok;
        Pid ->
            % Obtener PIDs de los procesos registrados
            case process_info(Pid, dictionary) of
                {dictionary, Dict} ->
                    % Detener cada componente
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
                    end;
                _ -> ok
            end,
            unregister(p2p_node),
            io:format("Nodo detenido~n"),
            ok
    end.
