-module(node_registry).
-export([start/0, stop/0, add_node/3, get_all_nodes/0, get_node/1]).

-record(node_info, {id, ip, port}).

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

% Inicializa el loop con mapa vacío
init() ->
    io:format("Registro de nodos iniciado~n"),
    loop(#{}).

% Loop principal del registro
loop(Nodes) ->
    receive
        {add_node, NodeId, Ip, Port} ->
            NodeInfo = #node_info{
                id = NodeId,
                ip = Ip,
                port = Port
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
        
        stop ->
            ok
    end.
%%debemos extender las demas funcionalidades (Ver parte 2)