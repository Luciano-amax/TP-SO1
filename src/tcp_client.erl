% Cliente TCP para conexiones salientes a otros nodos
-module(tcp_client).
-export([send_request/3, send_request/4]).

% Envia un request a un nodo remoto
send_request(Ip, Port, Request) ->
    send_request(Ip, Port, Request, 5000).

% Envia una request con timeout configurable
send_request(Ip, Port, Request, Timeout) ->
    case gen_tcp:connect(Ip, Port, [binary, {packet, 0}, {active, false}], Timeout) of
        {ok, Socket} ->
            ok = gen_tcp:send(Socket, Request),
            
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
