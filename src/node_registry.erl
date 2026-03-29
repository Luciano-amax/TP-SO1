-module(node_registry).
-include("config.hrl").
-export([start/0, stop/0, add_node/3, get_all_nodes/0, get_node/1, cleanup_inactive_nodes/0]).

-record(node_info, {id, ip, port, last_seen}).

% Arranca el proceso que guarda los nodos conocidos.
start() ->
    Pid = spawn(fun() -> init() end),
    register(node_registry, Pid),
    ok.

% Detiene el registro de nodos.
stop() ->
    case whereis(node_registry) of
        undefined -> ok;
        Pid -> 
            Pid ! stop,
            unregister(node_registry),
            ok
    end.

% Agrega o actualiza la informacion de un nodo visto en la red.
add_node(NodeId, Ip, Port) ->
    node_registry ! {add_node, NodeId, Ip, Port},
    ok.

% Devuelve todos los nodos conocidos en formato simple.
get_all_nodes() ->
    node_registry ! {get_all_nodes, self()},
    receive
        {all_nodes, Nodes} -> Nodes
    after 5000 ->
        []
    end.

% Busca un nodo puntual por su ID.
get_node(NodeId) ->
    node_registry ! {get_node, NodeId, self()},
    receive
        {node_found, NodeInfo} -> {ok, NodeInfo};
        {node_not_found} -> {error, not_found}
    after 5000 ->
        {error, timeout}
    end.

% Fuerza manualmente la limpieza de nodos inactivos.
cleanup_inactive_nodes() ->
    node_registry ! cleanup_inactive,
    ok.

% Programa la limpieza periodica y arranca con el mapa vacio.
init() ->
    erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup_inactive),
    loop(#{}).

% Mantiene el estado del registro y responde consultas.
loop(Nodes) ->
    receive
        {add_node, NodeId, Ip, Port} ->
            Now = erlang:system_time(second),
            NodeInfo = #node_info{
                id = NodeId,
                ip = Ip,
                port = Port,
                last_seen = Now
            },
            % Elimina cualquier nodo anterior con la misma IP:Puerto pero diferente ID
            CleanedNodes = maps:filter(fun(OldId, OldInfo) ->
                not ((OldInfo#node_info.ip =:= Ip) and 
                     (OldInfo#node_info.port =:= Port) and 
                     (OldId =/= NodeId))
            end, Nodes),
            NewNodes = maps:put(NodeId, NodeInfo, CleanedNodes),
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
            Now = erlang:system_time(second),
            % Se descartan los nodos que hace rato no mandan HELLO.
            NewNodes = maps:filter(fun(_NodeId, Info) ->
                TimeSinceLastSeen = Now - Info#node_info.last_seen,
                if 
                    TimeSinceLastSeen > ?NODE_TIMEOUT_SECONDS ->
                        io:format("Nodo inactivo: ~s~n", [Info#node_info.id]),
                        false;
                    true ->
                        true
                end
            end, Nodes),
            erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup_inactive),
            loop(NewNodes);
        
        stop ->
            ok
    end.
