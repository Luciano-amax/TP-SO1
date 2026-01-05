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
            case inet:peername(Socket) of
                {ok, {Address, Port}} ->
                    io:format("Nueva conexión desde ~p:~p~n", [Address, Port]);
                {error, _} ->
                    ok
            end,
            
            % Creamos un proceso nuevo para esta conexión (concurrencia)
            % Usamos spawn_link para detectar si el handler crashea
            spawn_link(fun() -> 
                handle_client_safe(Socket)
            end),
            % Seguimos aceptando más conexiones
            accept_loop(ListenSocket);
        {error, closed} ->
            ok;
        {error, Reason} ->
            io:format("Error en accept: ~p~n", [Reason]),
            accept_loop(ListenSocket)
    end.

% Wrapper con manejo de excepciones para handle_client
handle_client_safe(Socket) ->
    try
        handle_client(Socket)
    catch
        error:Reason:Stacktrace ->
            io:format("Error en handler de cliente: ~p~n~p~n", [Reason, Stacktrace]),
            gen_tcp:close(Socket);
        exit:Reason ->
            io:format("Handler de cliente termino: ~p~n", [Reason]),
            gen_tcp:close(Socket)
    end.

% Maneja la comunicación con un cliente conectado
handle_client(Socket) ->
    case gen_tcp:recv(Socket, 0, 30000) of
        {ok, Data} ->
            RequestStr = binary_to_list(Data),
            IsDownload = is_download_request(RequestStr),
            process_request(Socket, RequestStr, IsDownload),
            case IsDownload of
                false -> gen_tcp:close(Socket);
                true -> ok  % El socket se cierra despues de enviar el archivo
            end;
        {error, closed} ->
            % Cliente cerró la conexión, esto es normal
            gen_tcp:close(Socket);
        {error, timeout} ->
            io:format("Timeout esperando datos del cliente~n"),
            gen_tcp:close(Socket);
        {error, Reason} ->
            io:format("Error al recibir datos: ~p~n", [Reason]),
            gen_tcp:close(Socket)
    end.

% Verifica si es un request de descarga
is_download_request(RequestStr) ->
    Msg = string:trim(RequestStr),
    case string:tokens(Msg, " ") of
        ["DOWNLOAD_REQUEST" | _] -> true;
        _ -> false
    end.

% Procesa el request del cliente
process_request(Socket, RequestStr, IsDownload) ->
    Msg = string:trim(RequestStr),
    Tokens = string:tokens(Msg, " "),
    
    case Tokens of
        ["SEARCH_REQUEST", _NodeId, Pattern] ->
            handle_search_request(Socket, Pattern);
        ["DOWNLOAD_REQUEST", FileName] ->
            handle_download_request(Socket, FileName);
        _ ->
            io:format("Request no reconocido: ~s~n", [Msg]),
            case IsDownload of
                false -> gen_tcp:send(Socket, "ERROR\n");
                true -> ok
            end
    end.

% Maneja búsqueda de archivos
handle_search_request(Socket, Pattern) ->
    {ok, MyNodeId} = get_node_id(),
    Files = file_manager:search_files(Pattern),
    
    lists:foreach(fun({FileName, Size}) ->
        Response = io_lib:format("SEARCH_RESPONSE ~s ~s ~p~n", [MyNodeId, FileName, Size]),
        gen_tcp:send(Socket, Response)
    end, Files).

% Maneja descarga de archivos
handle_download_request(Socket, FileName) ->
    case file_manager:get_file(FileName) of
        {ok, Data, Size} ->
            case send_file(Socket, Data, Size) of
                ok ->
                    io:format("Archivo enviado exitosamente: ~s (~p bytes)~n", [FileName, Size]);
                {error, Reason} ->
                    io:format("Error al enviar archivo ~s: ~p~n", [FileName, Reason])
            end;
        {error, not_found} ->
            gen_tcp:send(Socket, <<112>>),
            io:format("Archivo no encontrado: ~s~n", [FileName])
    end,
    gen_tcp:close(Socket).

% Envia archivo por el socket (con chunks si es muy grande)
send_file(Socket, Data, Size) ->
    MB = 1024 * 1024,
    Code = <<101>>,
    SizeBin = <<Size:32/integer-big>>,
    
    if
        Size >= (MB * 4) ->
            % Archivo grande, enviar en chunks
            Msg = <<Code/binary, SizeBin/binary, MB:32/integer-big>>,
            case gen_tcp:send(Socket, Msg) of
                ok -> 
                    send_chunks(Socket, Data, 0);
                {error, Reason} ->
                    {error, Reason}
            end;
        true ->
            % Archivo pequeño, enviar todo de una vez
            Msg = <<Code/binary, SizeBin/binary, Data/binary>>,
            case gen_tcp:send(Socket, Msg) of
                ok -> ok;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

% Envia archivo en chunks de 1MB
send_chunks(Socket, Data, ChunkIndex) ->
    ChunkSize = 1024 * 1024,
    DataSize = byte_size(Data),
    
    if
        DataSize >= ChunkSize ->
            <<Chunk:ChunkSize/binary, Rest/binary>> = Data,
            Msg = <<111, ChunkIndex:16/integer-big, ChunkSize:32/integer-big, Chunk/binary>>,
            case gen_tcp:send(Socket, Msg) of
                ok ->
                    send_chunks(Socket, Rest, ChunkIndex + 1);
                {error, Reason} ->
                    {error, Reason}
            end;
        DataSize > 0 ->
            % Ultimo chunk (menor a 1MB)
            Msg = <<111, ChunkIndex:16/integer-big, DataSize:32/integer-big, Data/binary>>,
            case gen_tcp:send(Socket, Msg) of
                ok -> ok;
                {error, Reason} ->
                    {error, Reason}
            end;
        true ->
            ok
    end.

% Obtiene el NodeId del proceso p2p_node
get_node_id() ->
    case whereis(p2p_node) of
        undefined -> {ok, "unknown"};
        Pid ->
            case process_info(Pid, dictionary) of
                {dictionary, Dict} ->
                    case proplists:get_value(node_id, Dict) of
                        undefined -> {ok, "unknown"};
                        NodeId -> {ok, NodeId}
                    end;
                _ -> {ok, "unknown"}
            end
    end.
