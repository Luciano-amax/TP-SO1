% Servidor TCP para conexiones entrantes
-module(tcp_server).
-include("config.hrl").
-export([start/1, stop/0]).

% Arranca el servidor en el puerto indicado
start(Port) ->
    Pid = spawn(fun() -> init_server(Port) end),
    register(tcp_server, Pid),
    ok.

stop() ->
    case whereis(tcp_server) of
        undefined -> ok;
        Pid -> 
            Pid ! stop,
            unregister(tcp_server),
            ok
    end.

% Inicializa el socket de escucha
init_server(Port) ->
    % Opciones del socket:
    % 1 binary: trabajamos con binarios
    %2 {packet, 0}: sin empaquetado automático
    % 3 {active, false}: recibimos datos con gen_tcp:recv
    % 4 {reuseaddr, true}: permite reusar el puerto si se reinicia
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, 
                                                {packet, 0}, 
                                                {active, false}, 
                                                {reuseaddr, true}]),
    io:format("Servidor TCP escuchando en puerto ~p~n", [Port]),
    accept_loop(ListenSocket).

% Loop que acepta conexiones entrantes
accept_loop(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            % Obtenemos info del cliente que se conectó
            {ok, {Address, Port}} = inet:peername(Socket),
            io:format("Nueva conexión desde ~p:~p~n", [Address, Port]),
            
            % Creamos un proceso nuevo para esta conexión (concurrencia)
            spawn(fun() -> handle_client(Socket) end),
            % Seguimos aceptando más conexiones
            accept_loop(ListenSocket);
        {error, closed} ->
            ok
    end.

% Maneja la comunicación con un cliente conectado
handle_client(Socket) ->
    % Esperamos recibir datos (timeout 30 segundos)
    case gen_tcp:recv(Socket, 0, 30000) of
        {ok, Data} ->
            % Por ahora solo procesamos y respondemos
            Response = process_request(binary_to_list(Data)),
            gen_tcp:send(Socket, Response),
            gen_tcp:close(Socket);
        {error, _Reason} ->
            gen_tcp:close(Socket)
    end.

% Procesa el request del cliente
process_request(RequestStr) ->
    io:format("Request recibido: ~s", [RequestStr]),
    "OK\n".
