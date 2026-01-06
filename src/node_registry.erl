-module(node_registry).
-export([start/0, stop/0, add_node/3, get_all_nodes/0, get_node/1, cleanup_inactive_nodes/0]).

-record(node_info, {id, ip, port, last_seen}).

% Levanta el registro de nodos
start() ->
    Pid = spawn(fun() -> init() end),
    register(node_registry, Pid),
    ok.

% Detiene el registro
stop() ->
    case whereis(node_registry) of
        undefined -> ok;
        Pid -> 
            Pid ! stop,
            unregister(node_registry),
            ok
    end.

% Agrega o actualiza un nodo
add_node(NodeId, Ip, Port) ->
    node_registry ! {add_node, NodeId, Ip, Port},
    ok.

% Devuelve todos los nodos que conocemos
get_all_nodes() ->
    node_registry ! {get_all_nodes, self()},
    receive
        {all_nodes, Nodes} -> Nodes
    after 5000 ->
        []
    end.

% Obtiene un nodo específico por ID
get_node(NodeId) ->
    node_registry ! {get_node, NodeId, self()},
    receive
        {node_found, NodeInfo} -> {ok, NodeInfo};
        {node_not_found} -> {error, not_found}
    after 5000 ->
        {error, timeout}
    end.

% Solicita limpieza de nodos inactivos
cleanup_inactive_nodes() ->
    node_registry ! cleanup_inactive,
    ok.

% Inicializa el loop con mapa vacio
init() ->
    io:format("Registro de nodos iniciado~n"),
    % Inicia timer de limpieza cada 30 segundos
    erlang:send_after(30000, self(), cleanup_inactive),
    loop(#{}).

% Loop principal del registro
loop(Nodes) ->
    receive
        {add_node, NodeId, Ip, Port} ->
            % Timestamp actual en segundos
            Now = erlang:system_time(second),
            NodeInfo = #node_info{
                id = NodeId,
                ip = Ip,
                port = Port,
                last_seen = Now
            },
            NewNodes = maps:put(NodeId, NodeInfo, Nodes),
            io:format("Nodo agregado/actualizado: ~s (~p:~p)~n", [NodeId, Ip, Port]),
            loop(NewNodes);
        
        {get_all_nodes, From} ->
            NodeList = [{Info#node_info.id, 
                         Info#node_info.ip, 
                         Info#node_info.port} 
                        || Info <- maps:values(Nodes)],
            From ! {all_nodes, NodeList},
            loop(Nodes);
        
        {get_node, NodeId, From} ->
            case maps:find(NodeId, Nodes) of
                {ok, Info} ->
                    NodeData = {Info#node_info.id, 
                                Info#node_info.ip, 
                                Info#node_info.port},
                    From ! {node_found, NodeData};
                error ->
                    From ! {node_not_found}
            end,
            loop(Nodes);
        
        cleanup_inactive ->
            % Limpia nodos que no han enviado HELLO en 45+ segundos
            Now = erlang:system_time(second),
            NewNodes = maps:filter(fun(_NodeId, Info) ->
                TimeSinceLastSeen = Now - Info#node_info.last_seen,
                if 
                    TimeSinceLastSeen > 45 ->
                        io:format("Nodo inactivo eliminado: ~s (sin HELLO por ~p seg)~n", 
                                 [Info#node_info.id, TimeSinceLastSeen]),
                        false;  % Eliminar
                    true ->
                        true    % Mantener
                end
            end, Nodes),
            % Programa siguiente limpieza en 30 segundos
            erlang:send_after(30000, self(), cleanup_inactive),
            loop(NewNodes);
        
        stop ->
            ok
    end.
