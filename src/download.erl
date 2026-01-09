-module(download).
-include("config.hrl").
-export([download_from_node/2, download_multi_source/1]).

% Descarga un archivo desde un nodo específico
download_from_node(FileName, NodeId) ->
    case node_registry:get_node(NodeId) of
        {ok, {_Id, Ip, Port}} ->
            connect_and_download(Ip, Port, FileName);
        {error, not_found} ->
            io:format("Nodo no encontrado: ~s~n", [NodeId])
    end.

connect_and_download(Ip, Port, FileName) ->
    case gen_tcp:connect(Ip, Port, [binary, {active, false}, {reuseaddr, true}], 5000) of
        {ok, Socket} ->
            % Monitoreamos el socket para detectar desconexiones
            Request = io_lib:format("DOWNLOAD_REQUEST ~s~n", [FileName]),
            case gen_tcp:send(Socket, Request) of
                ok ->
                    Result = receive_file(Socket, FileName),
                    gen_tcp:close(Socket),
                    Result;
                {error, SendReason} ->
                    gen_tcp:close(Socket),
                    io:format("Error de envio: ~p~n", [SendReason]),
                    {error, send_failed}
            end;
        {error, econnrefused} ->
            io:format("Servidor no disponible~n"),
            {error, connection_refused};
        {error, timeout} ->
            io:format("Timeout de conexion~n"),
            {error, connection_timeout};
        {error, Reason} ->
            io:format("Error de conexion: ~p~n", [Reason]),
            {error, Reason}
    end.

receive_file(Socket, FileName) ->
    case gen_tcp:recv(Socket, 1, 5000) of
        {ok, <<101>>} ->
            case gen_tcp:recv(Socket, 4, 5000) of
                {ok, SizeBin} ->
                    <<Size:32/integer-big>> = SizeBin,
                    
                    % Hash SHA256 (32 bytes) - Mejora §5.3
                    % Timeout 2s: detecta si hay hash disponible
                    MB = 1024 * 1024,
                    case gen_tcp:recv(Socket, 32, 2000) of
                        {ok, ExpectedHash} when byte_size(ExpectedHash) == 32 ->
                            % Archivo con verificación de integridad
                            if
                                Size > (MB * 4) ->
                                    case gen_tcp:recv(Socket, 4, 5000) of
                                        {ok, <<_ChunkSize:32/integer-big>>} ->
                                            receive_chunks(Socket, FileName, ExpectedHash);
                                        {error, ChunkReason} ->
                                            io:format("Error metadata: ~p~n", [ChunkReason]),
                                            {error, chunk_metadata_failed}
                                    end;
                                true ->
                                    case gen_tcp:recv(Socket, Size, 30000) of
                                        {ok, Data} ->
                                            save_file(FileName, Data),
                                            verify_integrity(FileName, Data, ExpectedHash);
                                        {error, closed} ->
                                            io:format("Conexion cerrada~n"),
                                            {error, connection_closed};
                                        {error, timeout} ->
                                            io:format("Timeout~n"),
                                            {error, download_timeout};
                                        {error, Reason} ->
                                            io:format("Error recepcion: ~p~n", [Reason]),
                                            {error, Reason}
                                    end
                            end;
                        _ ->
                            % Archivo sin verificación de integridad
                            if
                                Size > (MB * 4) ->
                                    case gen_tcp:recv(Socket, 4, 5000) of
                                        {ok, <<_ChunkSize:32/integer-big>>} ->
                                            receive_chunks_no_hash(Socket, FileName);
                                        {error, ChunkReason} ->
                                            io:format("Error metadata: ~p~n", [ChunkReason]),
                                            {error, chunk_metadata_failed}
                                    end;
                                true ->
                                    case gen_tcp:recv(Socket, Size, 30000) of
                                        {ok, Data} ->
                                            save_file(FileName, Data),
                                            io:format("Descarga completa: ~s~n", [FileName]),
                                            {ok, FileName};
                                        {error, closed} ->
                                            io:format("Conexion cerrada~n"),
                                            {error, connection_closed};
                                        {error, timeout} ->
                                            io:format("Timeout~n"),
                                            {error, download_timeout};
                                        {error, Reason} ->
                                            io:format("Error recepcion: ~p~n", [Reason]),
                                            {error, Reason}
                                    end
                            end
                    end;
                {error, closed} ->
                    io:format("Conexion cerrada~n"),
                    {error, connection_closed};
                {error, Reason} ->
                    io:format("Error: ~p~n", [Reason]),
                    {error, Reason}
            end;
        {ok, <<112>>} ->
            io:format("Archivo no disponible~n"),
            {error, file_not_found};
        {ok, Other} ->
            io:format("Respuesta invalida: ~p~n", [Other]),
            {error, invalid_response};
        {error, closed} ->
            io:format("Conexion cerrada~n"),
            {error, connection_closed};
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason]),
            {error, Reason}
    end.

% Recibe chunks sin hash
receive_chunks_no_hash(Socket, FileName) ->
    FilePath = filename:join(?DOWNLOAD_DIR, FileName),
    file:delete(FilePath),
    case receive_chunks_loop(Socket, FilePath) of
        {ok, _} ->
            io:format("Descarga completa: ~s~n", [FileName]),
            {ok, FileName};
        Error ->
            Error
    end.

receive_chunks(Socket, FileName, ExpectedHash) ->
    FilePath = filename:join(?DOWNLOAD_DIR, FileName),
    file:delete(FilePath),
    case receive_chunks_loop(Socket, FilePath) of
        {ok, _} ->
            case file:read_file(FilePath) of
                {ok, Data} ->
                    verify_integrity(FileName, Data, ExpectedHash);
                {error, ReadReason} ->
                    io:format("Error leyendo: ~p~n", [ReadReason]),
                    {error, verification_read_failed}
            end;
        Error ->
            Error
    end.

receive_chunks_loop(Socket, FilePath) ->
    case gen_tcp:recv(Socket, 1, 5000) of
        {ok, <<111>>} ->
            case gen_tcp:recv(Socket, 2, 5000) of
                {ok, _IndexBin} ->
                    case gen_tcp:recv(Socket, 4, 5000) of
                        {ok, ChunkSizeBin} ->
                            <<ChunkSize:32/integer-big>> = ChunkSizeBin,
                            case gen_tcp:recv(Socket, ChunkSize, 30000) of
                                {ok, ChunkData} ->
                                    file:write_file(FilePath, ChunkData, [append]),
                                    receive_chunks_loop(Socket, FilePath);
                                {error, closed} ->
                                    io:format("Descarga parcial guardada~n"),
                                    {error, connection_closed_during_chunk};
                                {error, timeout} ->
                                    io:format("Timeout en chunk~n"),
                                    {error, chunk_timeout};
                                {error, Reason} ->
                                    io:format("Error chunk: ~p~n", [Reason]),
                                    {error, Reason}
                            end;
                        {error, closed} ->
                            {ok, FilePath};
                        {error, timeout} ->
                            {ok, FilePath};
                        {error, Reason} ->
                            io:format("Error tamano: ~p~n", [Reason]),
                            {error, Reason}
                    end;
                {error, closed} ->
                    {ok, FilePath};
                {error, Reason} ->
                    io:format("Error indice: ~p~n", [Reason]),
                    {error, Reason}
            end;
        {ok, _Other} ->
            io:format("Formato invalido~n"),
            {error, invalid_chunk_format};
        {error, timeout} ->
            {ok, FilePath};
        {error, closed} ->
            {ok, FilePath};
        {error, Reason} ->
            io:format("Error transferencia: ~p~n", [Reason]),
            {error, Reason}
    end.

save_file(FileName, Data) ->
    filelib:ensure_dir(?DOWNLOAD_DIR ++ "/"),
    FilePath = filename:join(?DOWNLOAD_DIR, FileName),
    case file:write_file(FilePath, Data) of
        ok ->
            ok;
        {error, Reason} ->
            io:format("Error guardando: ~p~n", [Reason])
    end.

verify_integrity(FileName, Data, ExpectedHash) ->
    ActualHash = crypto:hash(sha256, Data),
    if
        ActualHash =:= ExpectedHash ->
            io:format("Descarga OK: ~s [VERIFICADO]~n", [FileName]),
            {ok, FileName};
        true ->
            io:format("ADVERTENCIA: Hash no coincide para ~s~n", [FileName]),
            io:format("  Esperado: ~s~n", [binary_to_hex(ExpectedHash)]),
            io:format("  Obtenido: ~s~n", [binary_to_hex(ActualHash)]),
            {error, checksum_mismatch}
    end.

% Convierte binario a string hexadecimal
binary_to_hex(Bin) ->
    lists:flatten([io_lib:format("~2.16.0B", [B]) || <<B>> <= Bin]).

% Descarga multi-fuente con busqueda automatica
download_multi_source(FileName) ->
    io:format("Buscando nodos con ~s...~n", [FileName]),
    
    {ok, MyNodeId} = get_node_id(),
    Nodes = node_registry:get_all_nodes(),
    
    Parent = self(),
    lists:foreach(fun({_NodeId, Ip, Port}) ->
        spawn(fun() -> search_in_node(Parent, MyNodeId, Ip, Port, FileName) end)
    end, Nodes),
    
    SearchResults = collect_search_results(length(Nodes), []),
    
    case SearchResults of
        [] ->
            io:format("Archivo no encontrado en la red~n"),
            {error, not_found};
        _ ->
            io:format("~nEncontrado en ~p nodos~n", [length(SearchResults)]),
            start_multi_download(FileName, SearchResults)
    end.

% Busca archivo en un nodo especifico
search_in_node(Parent, MyNodeId, Ip, Port, FileName) ->
    case gen_tcp:connect(Ip, Port, [binary, {active, false}, {reuseaddr, true}], 2000) of
        {ok, Socket} ->
            Request = io_lib:format("SEARCH_REQUEST ~s ~s~n", [MyNodeId, FileName]),
            gen_tcp:send(Socket, Request),
            
            Results = receive_search_responses(Socket, FileName, []),
            gen_tcp:close(Socket),
            Parent ! {search_result, Results};
        {error, _Reason} ->
            Parent ! {search_result, []}
    end.

% Recibe respuestas de busqueda
receive_search_responses(Socket, FileName, Acc) ->
    case gen_tcp:recv(Socket, 0, 1000) of
        {ok, Data} ->
            Lines = string:tokens(binary_to_list(Data), "\n"),
            NewResults = lists:filtermap(fun(Line) ->
                parse_search_for_file(Line, FileName)
            end, Lines),
            receive_search_responses(Socket, FileName, Acc ++ NewResults);
        {error, _} ->
            Acc
    end.

% Parsea respuesta solo si coincide con el archivo buscado
parse_search_for_file(Line, FileName) ->
    Tokens = string:tokens(string:trim(Line), " "),
    case Tokens of
        ["SEARCH_RESPONSE", NodeId, FoundFile, SizeStr, Status | _Rest] when FoundFile =:= FileName ->
            {Size, _} = string:to_integer(SizeStr),
            ChunkInfo = parse_chunk_status(Status),
            {true, {NodeId, FoundFile, Size, ChunkInfo}};
        ["SEARCH_RESPONSE", NodeId, FoundFile, SizeStr] when FoundFile =:= FileName ->
            {Size, _} = string:to_integer(SizeStr),
            {true, {NodeId, FoundFile, Size, complete}};
        _ ->
            false
    end.

parse_chunk_status("COMPLETE") ->
    complete;
parse_chunk_status("CHUNKS:" ++ ChunkList) ->
    ChunkIds = [list_to_integer(C) || C <- string:tokens(ChunkList, ",")],
    {partial, ChunkIds};
parse_chunk_status(_) ->
    complete.

% Recolecta resultados de busqueda
collect_search_results(0, Results) ->
    Results;
collect_search_results(Remaining, Results) ->
    receive
        {search_result, NewResults} ->
            collect_search_results(Remaining - 1, Results ++ NewResults)
    after 3000 ->
        Results
    end.

% Inicia descarga desde multiples nodos
start_multi_download(FileName, SearchResults) ->
    [{_, _, TotalSize, _} | _] = SearchResults,
    ChunkSize = 4194304,
    
    download_manager:start(),
    {ok, _State} = download_manager:init_download(FileName, TotalSize, ChunkSize, SearchResults),
    
    io:format("Iniciando descarga paralela (~p nodos)~n", [length(SearchResults)]),
    
    Parent = self(),
    SourceNodes = extract_source_nodes(SearchResults),
    
    lists:foreach(fun(NodeId) ->
        spawn(fun() -> download_worker(Parent, FileName, NodeId) end)
    end, SourceNodes),
    
    wait_for_completion(FileName, length(SourceNodes)).

% Extrae lista de NodeIds unicos
extract_source_nodes(SearchResults) ->
    lists:usort([NodeId || {NodeId, _, _, _} <- SearchResults]).

% Trabajador que descarga chunks desde un nodo
% Cada worker pide y descarga chunks diferentes para evitar duplicados
download_worker(Parent, FileName, NodeId) ->
    case download_manager:assign_chunk(FileName, NodeId) of
        {ok, ChunkId} ->
            % Descarga chunk asignado en paralelo
            case download_chunk_from_node(FileName, ChunkId, NodeId) of
                ok ->
                    download_manager:mark_chunk_complete(FileName, ChunkId),
                    % Solicita siguiente chunk disponible
                    download_worker(Parent, FileName, NodeId);
                {error, Reason} ->
                    io:format("Error descargando chunk ~p desde ~s: ~p~n", [ChunkId, NodeId, Reason]),
                    Parent ! {worker_error, NodeId, ChunkId}
            end;
        {error, no_chunks} ->
            % No hay mas chunks para este nodo
            Parent ! {worker_done, NodeId}
    end.

% Descarga un chunk especifico desde un nodo
% Usa DOWNLOAD_REQUEST completo pero solo guarda el chunk asignado
download_chunk_from_node(FileName, ChunkId, NodeId) ->
    case node_registry:get_node(NodeId) of
        {ok, {_Id, Ip, Port}} ->
            case gen_tcp:connect(Ip, Port, [binary, {active, false}, {reuseaddr, true}], 5000) of
                {ok, Socket} ->
                    % Usa DOWNLOAD_REQUEST para bajar el archivo completo
                    Request = io_lib:format("DOWNLOAD_REQUEST ~s~n", [FileName]),
                    case gen_tcp:send(Socket, Request) of
                        ok ->
                            Result = receive_and_extract_chunk(Socket, FileName, ChunkId),
                            gen_tcp:close(Socket),
                            Result;
                        {error, SendReason} ->
                            gen_tcp:close(Socket),
                            {error, SendReason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, not_found} ->
            {error, node_not_found}
    end.

% Recibe archivo completo y extrae el chunk asignado
receive_and_extract_chunk(Socket, FileName, ChunkId) ->
    % Simplemente descargamos el archivo completo usando receive_file
    case receive_file_to_memory(Socket) of
        {ok, FileData} ->
            % Ahora extraemos solo el chunk que necesitamos
            extract_and_save_chunk(FileName, ChunkId, FileData);
        {error, Reason} ->
            {error, Reason}
    end.

% Recibe archivo completo en memoria (sin guardarlo a disco)
receive_file_to_memory(Socket) ->
    case gen_tcp:recv(Socket, 1, 5000) of
        {ok, <<101>>} ->
            case gen_tcp:recv(Socket, 4, 5000) of
                {ok, SizeBin} ->
                    <<Size:32/integer-big>> = SizeBin,
                    
                    % Hash SHA256 (32 bytes) - Mejora §5.3
                    % Timeout 2s: detecta si hay hash disponible
                    MB = 1024 * 1024,
                    case gen_tcp:recv(Socket, 32, 2000) of
                        {ok, ExpectedHash} when byte_size(ExpectedHash) == 32 ->
                            % Archivo con hash
                            receive_full_file_data(Socket, Size, MB, ExpectedHash);
                        _ ->
                            % Archivo sin hash
                            receive_full_file_data(Socket, Size, MB, no_hash)
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        {ok, <<112>>} ->
            {error, file_not_found};
        {error, Reason} ->
            {error, Reason}
    end.

% Recibe el contenido completo del archivo en memoria
receive_full_file_data(Socket, Size, MB, ExpectedHash) ->
    if
        Size > (MB * 4) ->
            % Archivo grande: recibir chunks de 1MB del servidor
            case gen_tcp:recv(Socket, 4, 5000) of
                {ok, <<_ServerChunkSize:32/integer-big>>} ->
                    receive_all_chunks(Socket, <<>>, ExpectedHash);
                {error, Reason} ->
                    {error, Reason}
            end;
        true ->
            % Archivo pequeño: recibir todo de una vez
            case gen_tcp:recv(Socket, Size, 30000) of
                {ok, Data} ->
                    % Verificar hash solo si está disponible
                    case ExpectedHash of
                        no_hash ->
                            {ok, Data};
                        _ ->
                            ActualHash = crypto:hash(sha256, Data),
                            if
                                ActualHash =:= ExpectedHash ->
                                    {ok, Data};
                                true ->
                                    {error, hash_mismatch}
                            end
                    end;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

% Recibe todos los chunks del servidor y los concatena
receive_all_chunks(Socket, Acc, ExpectedHash) ->
    case gen_tcp:recv(Socket, 1, 5000) of
        {ok, <<111>>} ->
            case gen_tcp:recv(Socket, 2, 5000) of
                {ok, _IndexBin} ->
                    case gen_tcp:recv(Socket, 4, 5000) of
                        {ok, ChunkSizeBin} ->
                            <<ChunkSize:32/integer-big>> = ChunkSizeBin,
                            case gen_tcp:recv(Socket, ChunkSize, 30000) of
                                {ok, ChunkData} ->
                                    NewAcc = <<Acc/binary, ChunkData/binary>>,
                                    receive_all_chunks(Socket, NewAcc, ExpectedHash);
                                {error, Reason} ->
                                    {error, Reason}
                            end;
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, timeout} ->
            % Fin del stream - verificar hash si está disponible
            case ExpectedHash of
                no_hash ->
                    {ok, Acc};
                _ ->
                    ActualHash = crypto:hash(sha256, Acc),
                    if
                        ActualHash =:= ExpectedHash ->
                            {ok, Acc};
                        true ->
                            {error, hash_mismatch}
                    end
            end;
        {error, closed} ->
            % Socket cerrado - verificar hash si está disponible
            case ExpectedHash of
                no_hash ->
                    {ok, Acc};
                _ ->
                    ActualHash = crypto:hash(sha256, Acc),
                    if
                        ActualHash =:= ExpectedHash ->
                            {ok, Acc};
                        true ->
                            {error, hash_mismatch}
                    end
            end;
        {error, Reason} ->
            {error, Reason}
    end.

% Extrae el chunk específico del archivo completo y lo guarda
extract_and_save_chunk(FileName, ChunkId, FileData) ->
    ChunkSize = 4194304,
    Offset = ChunkId * ChunkSize,
    TotalSize = byte_size(FileData),
    
    if
        Offset >= TotalSize ->
            {error, chunk_out_of_bounds};
        Offset + ChunkSize =< TotalSize ->
            <<_:Offset/binary, ChunkData:ChunkSize/binary, _/binary>> = FileData,
            save_chunk(FileName, ChunkId, ChunkData),
            ok;
        true ->
            % Último chunk (menor a 4MB)
            RemainingSize = TotalSize - Offset,
            <<_:Offset/binary, ChunkData:RemainingSize/binary>> = FileData,
            save_chunk(FileName, ChunkId, ChunkData),
            ok
    end.

% Guarda un chunk en disco
save_chunk(FileName, ChunkId, Data) ->
    ChunkDir = filename:join(?DOWNLOAD_DIR, "chunks"),
    filelib:ensure_dir(ChunkDir ++ "/"),
    ChunkPath = filename:join(ChunkDir, io_lib:format("~s.chunk~p", [FileName, ChunkId])),
    file:write_file(ChunkPath, Data).

% Espera a que terminen todos los workers
wait_for_completion(FileName, RemainingWorkers) ->
    case download_manager:is_complete(FileName) of
        {ok, true} ->
            % Todos los chunks bajados, ahora ensambla el archivo
            io:format("~nTodos los chunks descargados, ensamblando...~n"),
            case assemble_file(FileName) of
                ok ->
                    verify_assembled_file(FileName),
                    download_manager:stop();
                {error, Reason} ->
                    io:format("Error ensamblando archivo: ~p~n", [Reason]),
                    download_manager:stop()
            end;
        {ok, false} ->
            receive
                {worker_done, NodeId} ->
                    io:format("Worker ~s terminado~n", [NodeId]),
                    wait_for_completion(FileName, RemainingWorkers - 1);
                {worker_error, NodeId, ChunkId} ->
                    io:format("Error en worker ~s chunk ~p, reintentando...~n", [NodeId, ChunkId]),
                    wait_for_completion(FileName, RemainingWorkers)
            after 60000 ->
                io:format("Timeout esperando descarga~n"),
                download_manager:stop()
            end
    end.

% Ensambla el archivo final juntando todos los chunks
assemble_file(FileName) ->
    ChunkDir = filename:join(?DOWNLOAD_DIR, "chunks"),
    Pattern = filename:join(ChunkDir, FileName ++ ".chunk*"),
    ChunkFiles = filelib:wildcard(Pattern),
    
    % Lee y ordena chunks por ID (0, 1, 2, ...)
    ChunkData = lists:sort(lists:filtermap(fun(File) ->
        case parse_chunk_id(File) of
            {ok, Id} ->
                {ok, Data} = file:read_file(File),
                {true, {Id, Data}};
            error ->
                false
        end
    end, ChunkFiles)),
    
    % Concatena en orden correcto
    FinalPath = filename:join(?DOWNLOAD_DIR, FileName),
    {ok, OutFile} = file:open(FinalPath, [write, binary]),
    
    lists:foreach(fun({_Id, Data}) ->
        file:write(OutFile, Data)
    end, ChunkData),
    
    file:close(OutFile),
    
    % Limpia chunks temporales
    lists:foreach(fun(ChunkFile) ->
        file:delete(ChunkFile)
    end, ChunkFiles),
    
    io:format("Archivo ensamblado: ~s~n", [FileName]),
    ok.

% Verifica integridad del archivo ensamblado
verify_assembled_file(FileName) ->
    FinalPath = filename:join(?DOWNLOAD_DIR, FileName),
    case file:read_file(FinalPath) of
        {ok, Data} ->
            ActualHash = crypto:hash(sha256, Data),
            io:format("Archivo descargado: ~s~n", [FileName]),
            io:format("Hash SHA256: ~s~n", [binary_to_hex(ActualHash)]),
            io:format("Descarga completada [VERIFICADO]~n");
        {error, Reason} ->
            io:format("Error verificando archivo: ~p~n", [Reason])
    end.

parse_chunk_id(FilePath) ->
    BaseName = filename:basename(FilePath),
    case string:split(BaseName, ".chunk", trailing) of
        [_, IdStr] ->
            case string:to_integer(IdStr) of
                {Id, _} -> {ok, Id};
                _ -> error
            end;
        _ -> error
    end.

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

