% Registro de nodos conocidos
-module(node_registry).
-export([start/0, stop/0, add_node/3, get_all_nodes/0]).

% Info de cada nodo: id, ip y puerto
-record(node_info, {
    id,
    ip,
    port
}).

% Levanta el proceso del registro
start() ->
    Pid = spawn(fun() -> init() end),
    register(node_registry, Pid),
    ok.

stop() ->
    case whereis(node_registry) of
        undefined -> ok;
        Pid -> 
            Pid ! stop,
            unregister(node_registry),
            ok
    end.

% Agrega un nodo al registro
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

% Arranca con el mapa de nodos vacío
init() ->
    io:format("Registro de nodos iniciado~n"),
    loop(#{}).

% Loop principal que mantiene el estado
loop(Nodes) ->
    receive
        {add_node, NodeId, Ip, Port} ->
            NodeInfo = #node_info{
                id = NodeId,
                ip = Ip,
                port = Port
            },
            % Guardamos o actualizamos el nodo en el mapa
            NewNodes = maps:put(NodeId, NodeInfo, Nodes),
            io:format("Nodo agregado: ~s~n", [NodeId]),
            loop(NewNodes);
        
        {get_all_nodes, From} ->
            % Convertimos el mapa a lista de records
            NodeList = maps:values(Nodes),
            From ! {all_nodes, NodeList},
            loop(Nodes);
        
        stop ->
            ok
    end.
%%debemos extender las demas funcionalidades (Ver parte 2)