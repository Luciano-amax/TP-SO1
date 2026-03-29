-module(p2p_node).
-export([start/1, stop/0]).
-include("config.hrl").

% Inicia el nodo P2P completo
start(TcpPort) ->
    Caller = self(),
    Pid = spawn(fun() -> init_node(TcpPort, Caller) end),
    receive
        {node_started, Pid} ->
            wait_until_stopped();
        {node_start_error, Reason} ->
            io:format("No se pudo iniciar el nodo: ~p~n", [Reason]),
            {error, Reason}
    end.

% Inicializa los componentes del nodo y deja corriendo al coordinador.
init_node(TcpPort, Caller) ->
    catch unregister(p2p_node),
    catch unregister(file_manager),
    catch unregister(node_registry),
    catch unregister(tcp_server),
    catch unregister(discovery),
    catch unregister(hello_broadcast),

    register(p2p_node, self()),

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

    put(node_id, NodeId),
    put(file_manager, whereis(file_manager)),
    put(node_registry, whereis(node_registry)),
    put(tcp_server, whereis(tcp_server)),
    put(discovery, whereis(discovery)),
    put(hello_broadcast, whereis(hello_broadcast)),

    % La CLI corre en un proceso separado para no bloquear al coordinador.
    CliPid = spawn(fun() -> cli:start() end),
    put(cli, CliPid),

    io:format("~nNodo iniciado: ~s (puerto ~p)~n~n", [NodeId, TcpPort]),
    Caller ! {node_started, self()},
    node_loop(Caller).

% Espera hasta que el coordinador confirme que termino el apagado.
wait_until_stopped() ->
    receive
        node_stopped ->
            ok
    end.

% Mantiene vivo al coordinador y recibe mensajes de control.
node_loop(Caller) ->
    receive
        stop ->
            shutdown_node(),
            Caller ! node_stopped;
        {cli_stopped, _CliPid} ->
            node_loop(Caller);
        _Other ->
            node_loop(Caller)
    end.

% Detiene el nodo y todos sus componentes
stop() ->
    % Detener todos los procesos hijos
    case whereis(p2p_node) of
        undefined -> 
            io:format("Nodo ya detenido~n"),
            ok;
        Pid ->
            Pid ! stop,
            ok
    end.

% Cierra los procesos hijos registrados en el estado del coordinador.
shutdown_node() ->
    case process_info(self(), dictionary) of
        {dictionary, Dict} ->
            % La salida se hace de forma ordenada para no dejar procesos sueltos.
            case proplists:get_value(cli, Dict) of
                undefined -> ok;
                CliPid -> exit(CliPid, shutdown)
            end,
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
        _ ->
            ok
    end,
    unregister(p2p_node),
    io:format("Nodo detenido~n"),
    ok.
