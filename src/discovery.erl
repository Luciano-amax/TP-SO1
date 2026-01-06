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
        {ok, FinalId} ->
            % Guardamos los IDs que solicitamos durante el consenso
            RequestedIds = sets:from_list([FinalId]),
            Pid = spawn(fun() -> loop(Socket, FinalId, TcpPort, RequestedIds) end),
            register(discovery, Pid),
            io:format("ID consensuado: ~s~n", [FinalId]),
            {ok, FinalId};
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
    request_node_id_internal(Socket, UdpPort, NodeId, sets:new()).

% Version con acumulador de IDs solicitados
request_node_id_internal(Socket, UdpPort, NodeId, RequestedIds) ->
    % Enviamos NAME_REQUEST con nuestro ID propuesto
    Msg = io_lib:format("NAME_REQUEST ~s\n", [NodeId]),
    gen_udp:send(Socket, {255, 255, 255, 255}, UdpPort, Msg),
    
    io:format("Solicitando ID: ~s~n", [NodeId]),
    
    % Agregamos este ID al conjunto de IDs solicitados
    NewRequestedIds = sets:add_element(NodeId, RequestedIds),
    
    % Calculamos el deadline una sola vez aca
    % Bug arreglado: antes haciamos receive recursivo que reiniciaba el timeout
    % cada vez que llegaba un mensaje (incluso nuestro propio broadcast)
    case wait_for_rejection(Socket, NodeId, UdpPort, erlang:monotonic_time(millisecond) + 10000, NewRequestedIds) of
        ok ->
            io:format("ID consensuado: ~s~n", [NodeId]),
            {ok, NodeId};
        {ok, NewNodeId} ->
            {ok, NewNodeId}
    end.

% Espera por rechazos sin reiniciar el timeout
% Esta función maneja mensajes pero mantiene el deadline original
wait_for_rejection(Socket, NodeId, UdpPort, Deadline, RequestedIds) ->
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
                            % Espera random entre 2-10 segundos
                            timer:sleep(2000 + rand:uniform(8000)),
                            NewId = generate_random_id(4),
                            request_node_id_internal(Socket, UdpPort, NewId, RequestedIds);
                        _ ->
                            % Es otro mensaje (ej: nuestro propio broadcast o de otro nodo)
                            % Lo ignoramos y seguimos esperando con el MISMO deadline
                            wait_for_rejection(Socket, NodeId, UdpPort, Deadline, RequestedIds)
                    end
            after TimeLeft ->
                % Timeout final alcanzado
                ok
            end
    end.

% Loop principal del proceso de descubrimiento
loop(Socket, NodeId, TcpPort, RequestedIds) ->
    receive
        {udp, Socket, SrcIp, SrcPort, Data} ->
            handle_message(Socket, SrcIp, SrcPort, binary_to_list(Data), NodeId, RequestedIds),
            loop(Socket, NodeId, TcpPort, RequestedIds);
        
        {get_id, From} ->
            From ! {node_id, NodeId},
            loop(Socket, NodeId, TcpPort, RequestedIds);
        
        stop ->
            gen_udp:close(Socket),
            ok
    end.

% Maneja mensajes UDP recibidos
handle_message(Socket, SrcIp, SrcPort, Message, MyNodeId, RequestedIds) ->
    case parse_message(Message) of
        {name_request, RequestedId} ->
            % Condicion 1: El ID solicitado coincide con nuestro ID actual
            Condition1 = (RequestedId == MyNodeId),
            % Condicion 2: El ID fue solicitado por nosotros anteriormente
            Condition2 = sets:is_element(RequestedId, RequestedIds),
            
            % Si alguna condicion se cumple, rechazamos
            if Condition1 orelse Condition2 ->
                Reply = io_lib:format("INVALID_NAME ~s\n", [RequestedId]),
                gen_udp:send(Socket, SrcIp, SrcPort, Reply),
                io:format("ID ~s rechazado (ya en uso o solicitado)~n", [RequestedId]);
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
