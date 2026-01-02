-module(hello_broadcast).
-export([start/2, stop/0]).
-include("config.hrl").

% Inicia broadcasts HELLO periódicos
start(NodeId, TcpPort) ->
    catch unregister(hello_broadcast),
    
    {ok, Socket} = gen_udp:open(?UDP_PORT, [{active, true}, 
                                             {broadcast, true}, 
                                             {ip, {0,0,0,0}}, 
                                             {reuseaddr, true}]),
    
    HelloMsg = io_lib:format("HELLO ~s ~w\n", [NodeId, TcpPort]),
    
    Pid = spawn(fun() -> hello_loop(Socket, HelloMsg, NodeId) end),
    register(hello_broadcast, Pid),
    gen_udp:controlling_process(Socket, Pid),
    
    ok.

% Detiene broadcasts
stop() ->
    case whereis(hello_broadcast) of
        undefined -> ok;
        Pid -> exit(Pid, shutdown)
    end.

% Loop que envía HELLO y procesa recepción
hello_loop(Socket, HelloMsg, MyNodeId) ->
    gen_udp:send(Socket, {255, 255, 255, 255}, ?UDP_PORT, HelloMsg),
    timer:sleep(?HELLO_INTERVAL),
    
    flush_messages(Socket, MyNodeId),
    
    hello_loop(Socket, HelloMsg, MyNodeId).

% Procesa mensajes UDP acumulados
flush_messages(Socket, MyNodeId) ->
    receive
        {udp, Socket, SrcIp, _SrcPort, Data} ->
            Msg = string:trim(Data),
            Tokens = string:tokens(Msg, " "),
            
            case Tokens of
                ["HELLO", NodeId, TcpPortStr] ->
                    if 
                        NodeId /= MyNodeId ->
                            {TcpPort, _} = string:to_integer(TcpPortStr),
                            node_registry:add_node(NodeId, SrcIp, TcpPort),
                            io:format("Nodo agregado/actualizado: ~s (~p:~w)~n", [NodeId, SrcIp, TcpPort]);
                        true ->
                            ok
                    end;
                _ ->
                    ok
            end,
            flush_messages(Socket, MyNodeId)
    after 0 ->
        ok
    end.
