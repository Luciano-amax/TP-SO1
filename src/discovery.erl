% Módulo de descubrimiento y consenso de ID único
-module(discovery).
-include("config.hrl").
-export([start/2, stop/0, get_node_id/0, request_node_id/0]).

request_node_id() ->
    UdpPort = ?UDP_PORT,
    % Abrimos el socket UDP para broadcasts
    case gen_udp:open(UdpPort, [binary, 
                                 {broadcast, true}, 
                                 {reuseaddr, true}, 
                                 {active, true}]) of
        {ok, Socket} ->
            % Generamos un ID aleatorio
            NodeId = generate_random_id(4),
            
            % Pedimos consenso a la red
            case request_node_id_internal(Socket, UdpPort, NodeId) of
                {ok, FinalId} ->
                    gen_udp:close(Socket),
                    {ok, FinalId};
                {error, Reason} ->
                    gen_udp:close(Socket),
                    {error, Reason}
            end;
        {error, Reason} ->
            io:format("Error al abrir socket UDP: ~p. Reintentando...~n", [Reason]),
            timer:sleep(1000),
            request_node_id()
    end.

% Inicia el proceso de descubrimiento
% UdpPort: puerto para escuchar broadcasts def macro 
% TcpPort: puerto TCP de este nodo
start(UdpPort, TcpPort) ->
    % Abrimos el socket UDP para broadcasts
    {ok, Socket} = gen_udp:open(UdpPort, [binary, 
                                           {broadcast, true}, 
                                           {reuseaddr, true}, 
                                           {active, true}]),
    
    % Generamos un ID aleatorio
    NodeId = generate_random_id(4),
    
    % Pedimos consenso a la red
    case request_node_id_internal(Socket, UdpPort, NodeId) of
        ok ->
            Pid = spawn(fun() -> loop(Socket, NodeId, TcpPort) end),
            register(discovery, Pid),
            io:format("ID consensuado: ~s~n", [NodeId]),
            {ok, NodeId};
        {error, Reason} ->
            gen_udp:close(Socket),
            {error, Reason}
    end.

stop() ->
    case whereis(discovery) of
        undefined -> ok;
        Pid -> 
            Pid ! stop,
            unregister(discovery),
            ok
    end.

get_node_id() ->
    case whereis(discovery) of
        undefined -> undefined;
        Pid ->
            Pid ! {get_id, self()},
            receive
                {node_id, Id} -> Id
            after 1000 ->
                undefined
            end
    end.

% Genera un ID aleatorio de N caracteres
generate_random_id(N) ->
    Chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789",
    lists:map(fun(_) -> 
        lists:nth(rand:uniform(length(Chars)), Chars) 
    end, lists:seq(1, N)).

% Solicita un ID a la red usando consenso
request_node_id_internal(Socket, UdpPort, NodeId) ->
    % Enviamos NAME_REQUEST con nuestro ID propuesto
    Msg = io_lib:format("NAME_REQUEST ~s\n", [NodeId]),
    gen_udp:send(Socket, {255, 255, 255, 255}, UdpPort, Msg),
    
    io:format("Solicitando ID: ~s~n", [NodeId]),
    
    % Calculamos el deadline una sola vez acá
    % Bug arreglado: antes hacíamos receive recursivo que reiniciaba el timeout
    % cada vez que llegaba un mensaje (incluso nuestro propio broadcast)
    case wait_for_rejection(Socket, NodeId, UdpPort, erlang:monotonic_time(millisecond) + 10000) of
        ok ->
            io:format("ID consensuado: ~s~n", [NodeId]),
            {ok, NodeId};
        {ok, NewNodeId} ->
            {ok, NewNodeId}
    end.

% Espera por rechazos sin reiniciar el timeout
% Esta función maneja mensajes pero mantiene el deadline original
wait_for_rejection(Socket, NodeId, UdpPort, Deadline) ->
    % Calculamos cuánto tiempo nos queda hasta el deadline
    TimeLeft = Deadline - erlang:monotonic_time(millisecond),
    if 
        TimeLeft =< 0 ->
            % Se acabó el tiempo, nadie rechazó el ID
            ok;
        true ->
            receive
                {udp, Socket, _Ip, _Port, Data} ->
                    case parse_message(binary_to_list(Data)) of
                        {invalid_name, RecvId} when RecvId == NodeId ->
                            % Nos rechazaron, probamos con otro ID
                            io:format("ID ~s ya existe, reintentando...~n", [NodeId]),
                            timer:sleep(rand:uniform(2000) + 1000),
                            NewId = generate_random_id(4),
                            request_node_id_internal(Socket, UdpPort, NewId);
                        _ ->
                            % Es otro mensaje (ej: nuestro propio broadcast o de otro nodo)
                            % Lo ignoramos y seguimos esperando con el MISMO deadline
                            wait_for_rejection(Socket, NodeId, UdpPort, Deadline)
                    end
            after TimeLeft ->
                % Timeout final alcanzado
                ok
            end
    end.

% Loop principal del proceso de descubrimiento
loop(Socket, NodeId, TcpPort) ->
    receive
        {udp, Socket, SrcIp, SrcPort, Data} ->
            handle_message(Socket, SrcIp, SrcPort, binary_to_list(Data), NodeId),
            loop(Socket, NodeId, TcpPort);
        
        {get_id, From} ->
            From ! {node_id, NodeId},
            loop(Socket, NodeId, TcpPort);
        
        stop ->
            gen_udp:close(Socket),
            ok
    end.

% Maneja mensajes UDP recibidos
handle_message(Socket, SrcIp, SrcPort, Message, MyNodeId) ->
    case parse_message(Message) of
        {name_request, RequestedId} ->
            % Si alguien pide un ID que ya usamos, lo rechazamos
            if RequestedId == MyNodeId ->
                Reply = io_lib:format("INVALID_NAME ~s\n", [RequestedId]),
                gen_udp:send(Socket, SrcIp, SrcPort, Reply),
                io:format("ID ~s rechazado (ya en uso)~n", [RequestedId]);
            true ->
                ok
            end;
        _ ->
            ok
    end.

% Parsea los mensajes UDP
parse_message(Message) ->
    case string:tokens(string:trim(Message), " \n\r") of
        ["NAME_REQUEST", Id] ->
            {name_request, Id};
        ["INVALID_NAME", Id] ->
            {invalid_name, Id};
        _ ->
            {unknown, Message}
    end.
