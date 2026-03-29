-module(tcp_server).
-include("config.hrl").
-export([start/1, stop/0]).

% Arranca el servidor TCP que escucha pedidos entrantes.
start(Port) ->
    Pid = spawn(fun() -> init_server(Port) end),
    register(tcp_server, Pid),
    ok.

% Detiene el servidor TCP si estaba levantado.
stop() ->
    case whereis(tcp_server) of
        undefined -> ok;
        Pid -> 
            Pid ! stop,
            unregister(tcp_server),
            ok
    end.

% Abre el socket de escucha en el puerto indicado.
init_server(Port) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, 
                                                {packet, 0}, 
                                                {active, false}, 
                                                {reuseaddr, true}]),
    accept_loop(ListenSocket).

% Acepta clientes y crea un proceso para cada conexion.
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

% Encierra el handler real para que un error no tire todo el servidor.
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

% Lee el pedido del cliente y decide como cerrar la conexion.
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
        ["DOWNLOAD_CHUNK" | _] -> true;
        _ -> false
    end.

% Procesa el request del cliente
process_request(Socket, RequestStr, IsDownload) ->
    Msg = string:trim(RequestStr),
    Tokens = string:tokens(Msg, " "),
    
    case Tokens of
        ["SEARCH_REQUEST", _NodeId, Pattern] ->
            handle_search_request(Socket, Pattern);
        ["CHECKSUM_REQUEST", FileName] ->
            handle_checksum_request(Socket, FileName);
        ["DOWNLOAD_REQUEST", FileName] ->
            handle_download_request(Socket, FileName);
        ["DOWNLOAD_CHUNK", FileName, ChunkIdStr] ->
            {ChunkId, _} = string:to_integer(ChunkIdStr),
            handle_chunk_request(Socket, FileName, ChunkId);
        _ ->
            io:format("Request no reconocido: ~s~n", [Msg]),
            case IsDownload of
                false -> gen_tcp:send(Socket, "ERROR\n");
                true -> ok
            end
    end.

% Maneja busqueda de archivos
handle_search_request(Socket, Pattern) ->
    {ok, MyNodeId} = get_node_id(),
    Files = file_manager:search_files(Pattern),
    
    lists:foreach(fun({FileName, Size}) ->
        Status = format_chunk_status(file_manager:get_available_chunks(FileName)),
        Response = io_lib:format("SEARCH_RESPONSE ~s ~s ~p ~s~n", [MyNodeId, FileName, Size, Status]),
        gen_tcp:send(Socket, Response)
    end, Files).

% Esta extension devuelve el sha256 del archivo para verificar integridad.
handle_checksum_request(Socket, FileName) ->
    case file_manager:get_file(FileName) of
        {ok, Data, _Size} ->
            HashHex = binary_to_hex(crypto:hash(sha256, Data)),
            Response = io_lib:format("CHECKSUM_RESPONSE ~s~n", [HashHex]),
            gen_tcp:send(Socket, Response);
        {error, not_found} ->
            gen_tcp:send(Socket, "CHECKSUM_NOTFOUND\n")
    end.

% Atiende la descarga simple usando el protocolo base.
handle_download_request(Socket, FileName) ->
    case file_manager:get_file(FileName) of
        {ok, Data, Size} ->
            send_file(Socket, Data, Size);
        {error, not_found} ->
            gen_tcp:send(Socket, <<?CODE_NOTFOUND>>)
    end,
    gen_tcp:close(Socket).

% Maneja descarga de un chunk especifico
handle_chunk_request(Socket, FileName, ChunkId) ->
    case get_chunk_data(FileName, ChunkId) of
        {ok, Data} ->
            Size = byte_size(Data),
            % Para la descarga por chunk se envia solo el fragmento pedido.
            Msg = <<?CODE_OK, Size:32/integer-big, Data/binary>>,
            gen_tcp:send(Socket, Msg);
        {error, not_found} ->
            gen_tcp:send(Socket, <<?CODE_NOTFOUND>>)
    end,
    gen_tcp:close(Socket).

% Obtiene datos de un chunk especifico
get_chunk_data(FileName, ChunkId) ->
    ChunkDir = filename:join(?DOWNLOAD_DIR, "chunks"),
    ChunkPath = filename:join(ChunkDir, io_lib:format("~s.chunk~p", [FileName, ChunkId])),
    
    case file:read_file(ChunkPath) of
        {ok, Data} ->
            {ok, Data};
        {error, enoent} ->
            case file_manager:get_file(FileName) of
                {ok, FileData, _Size} ->
                    extract_chunk_from_file(FileData, ChunkId);
                {error, not_found} ->
                    {error, not_found}
            end
    end.

% Extrae un chunk de un archivo completo
extract_chunk_from_file(FileData, ChunkId) ->
    ChunkSize = ?CHUNK_SIZE,
    Offset = ChunkId * ChunkSize,
    TotalSize = byte_size(FileData),
    
    if
        Offset >= TotalSize ->
            {error, not_found};
        Offset + ChunkSize =< TotalSize ->
            <<_:Offset/binary, Chunk:ChunkSize/binary, _/binary>> = FileData,
            {ok, Chunk};
        true ->
            RemainingSize = TotalSize - Offset,
            <<_:Offset/binary, Chunk:RemainingSize/binary>> = FileData,
            {ok, Chunk}
    end.

% Envia el archivo segun el formato base definido para la descarga.
send_file(Socket, Data, Size) ->
    Code = <<?CODE_OK>>,
    SizeBin = <<Size:32/integer-big>>,
    
    if
        % El protocolo base usa chunks solo cuando el archivo supera los 4MB.
        Size > ?LARGE_FILE_THRESHOLD ->
            % Para el DOWNLOAD_REQUEST base se usa un tamaño de transferencia
            % compatible con el campo de 16 bits del protocolo.
            TransferChunkSize = transfer_chunk_size(),
            Msg = <<Code/binary, SizeBin/binary, TransferChunkSize:32/integer-big>>,
            case gen_tcp:send(Socket, Msg) of
                ok -> 
                    send_chunks(Socket, Data, 0, TransferChunkSize);
                {error, Reason} ->
                    {error, Reason}
            end;
        true ->
            Msg = <<Code/binary, SizeBin/binary, Data/binary>>,
            case gen_tcp:send(Socket, Msg) of
                ok -> ok;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

% Envia archivo grande como secuencia de chunks compatibles con el
% campo de tamaño real de 16 bits del protocolo base.
send_chunks(Socket, Data, ChunkIndex, ChunkSize) ->
    DataSize = byte_size(Data),
    
    if
        DataSize >= ChunkSize ->
            <<Chunk:ChunkSize/binary, Rest/binary>> = Data,
            % El tamaño real de cada chunk se envía en 16 bits como indica el TP.
            Msg = <<?CODE_CHUNK, ChunkIndex:16/integer-big, ChunkSize:16/integer-big, Chunk/binary>>,
            case gen_tcp:send(Socket, Msg) of
                ok ->
                    send_chunks(Socket, Rest, ChunkIndex + 1, ChunkSize);
                {error, Reason} ->
                    {error, Reason}
            end;
        DataSize > 0 ->
            % Ultimo chunk (menor a 4MB)
            Msg = <<?CODE_CHUNK, ChunkIndex:16/integer-big, DataSize:16/integer-big, Data/binary>>,
            case gen_tcp:send(Socket, Msg) of
                ok -> ok;
                {error, Reason} ->
                    {error, Reason}
            end;
        true ->
            ok
    end.

% Fija un tamano compatible con el campo real de 16 bits del chunk.
transfer_chunk_size() ->
    65535.

% Convierte un hash binario a texto hexadecimal.
binary_to_hex(Bin) ->
    lists:flatten([io_lib:format("~2.16.0B", [Byte]) || <<Byte>> <= Bin]).

% Traduce el estado del archivo al formato que viaja en SEARCH_RESPONSE.
format_chunk_status({complete, _TotalChunks}) ->
    "COMPLETE";
format_chunk_status({partial, ChunkIds}) ->
    ChunkText = string:join([integer_to_list(Id) || Id <- ChunkIds], ","),
    "CHUNKS:" ++ ChunkText;
format_chunk_status(not_found) ->
    "COMPLETE".

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
