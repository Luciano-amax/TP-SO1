% Cliente TCP para conexiones salientes a otros nodos
-module(tcp_client).
-include("config.hrl").
-export([send_request/3, send_request/4]).

% Envia un request a un nodo remoto
% Usa el timeout por defecto para pedidos cortos.
send_request(Ip, Port, Request) ->
    send_request(Ip, Port, Request, ?SEARCH_TIMEOUT).

% Envia una request con timeout configurable
% Abre la conexion, manda el pedido y espera una sola respuesta.
send_request(Ip, Port, Request, Timeout) ->
    case gen_tcp:connect(Ip, Port, [binary, {packet, 0}, {active, false}], Timeout) of
        {ok, Socket} ->
            ok = gen_tcp:send(Socket, Request),
            
            % Si el remoto responde, devolvemos el binario tal cual vino.
            Result = case gen_tcp:recv(Socket, 0, Timeout) of
                {ok, Data} ->
                    {ok, Data};
                {error, Reason} ->
                    {error, Reason}
            end,
            
            gen_tcp:close(Socket),
            Result;
        
        {error, Reason} ->
            {error, Reason}
    end.
