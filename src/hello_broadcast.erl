-module(hello_broadcast).
-export([start/2, stop/0]).
-include("config.hrl").

% Inicia broadcasts HELLO periódicos
start(NodeId, TcpPort) ->
    catch unregister(hello_broadcast),
    Pid = spawn(fun() -> init_sender(NodeId, TcpPort) end),
    register(hello_broadcast, Pid),
    ok.

% Detiene broadcasts
stop() ->
    case whereis(hello_broadcast) of
        undefined -> ok;
        Pid -> exit(Pid, shutdown)
    end.

init_sender(NodeId, TcpPort) ->
    {ok, Socket} = gen_udp:open(0, [binary, {broadcast, true}]),
    HelloMsg = io_lib:format("HELLO ~s ~w\n", [NodeId, TcpPort]),
    hello_loop(Socket, HelloMsg).

% Loop que envía HELLO periódicamente
hello_loop(Socket, HelloMsg) ->
    gen_udp:send(Socket, ?BROADCAST_ADDR, ?UDP_PORT, HelloMsg),
    
    % Intervalo aleatorio entre 15-20 segundos
    Interval = ?HELLO_INTERVAL_MIN + rand:uniform(?HELLO_INTERVAL_MAX - ?HELLO_INTERVAL_MIN),
    timer:sleep(Interval),
    hello_loop(Socket, HelloMsg).
