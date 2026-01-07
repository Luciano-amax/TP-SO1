-module(tcp_server).
-include("config.hrl").
-export([start/1, stop/0]).

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

init_server(Port) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, 
                                                {packet, 0}, 
                                                {active, false}, 
                                                {reuseaddr, true}]),
    accept_loop(ListenSocket).

accept_loop(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            spawn_link(fun() -> 
                handle_client_safe(Socket)
            end),
            accept_loop(ListenSocket);
        {error, closed} ->
            ok;
        {error, Reason} ->
            io:format("Error accept: ~p~n", [Reason]),
            accept_loop(ListenSocket)
    end.

handle_client_safe(Socket) ->
    try
        handle_client(Socket)
    catch
        error:Reason:Stacktrace ->
            io:format("Error handler: ~p~n~p~n", [Reason, Stacktrace]),
            gen_tcp:close(Socket);
        exit:Reason ->
            io:format("Handler termino: ~p~n", [Reason]),
            gen_tcp:close(Socket)
    end.

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

handle_download_request(Socket, FileName) ->
    case file_manager:get_file(FileName) of
        {ok, Data, Size} ->
            send_file(Socket, Data, Size);
        {error, not_found} ->
            gen_tcp:send(Socket, <<112>>)
    end,
    gen_tcp:close(Socket).

send_file(Socket, Data, Size) ->
    MB = 1024 * 1024,
    Code = <<101>>,
    SizeBin = <<Size:32/integer-big>>,
    Hash = crypto:hash(sha256, Data),
    
    if
        Size >= (MB * 4) ->
            Msg = <<Code/binary, SizeBin/binary, Hash:32/binary, MB:32/integer-big>>,
            case gen_tcp:send(Socket, Msg) of
                ok -> 
                    send_chunks(Socket, Data, 0);
                {error, Reason} ->
                    {error, Reason}
            end;
        true ->
            Msg = <<Code/binary, SizeBin/binary, Hash:32/binary, Data/binary>>,
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
