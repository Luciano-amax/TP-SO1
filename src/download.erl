-module(download).
-include("config.hrl").
-export([download_from_node/2, download_multi_source/1]).

% Descarga simple desde un nodo puntual usando el protocolo base.
download_from_node(FileName, NodeId) ->
    case node_registry:get_node(NodeId) of
        {ok, {_Id, Ip, Port}} ->
            connect_and_download(Ip, Port, FileName);
        {error, not_found} ->
            io:format("Nodo no encontrado: ~s~n", [NodeId])
    end.

% Abre la conexion TCP y delega la recepcion del archivo.
connect_and_download(Ip, Port, FileName) ->
    case gen_tcp:connect(Ip, Port, [binary, {active, false}, {reuseaddr, true}], ?TCP_CONNECT_TIMEOUT) of
        {ok, Socket} ->
            Request = io_lib:format("DOWNLOAD_REQUEST ~s~n", [FileName]),
            case gen_tcp:send(Socket, Request) of
                ok ->
                    Result = receive_file(Socket, FileName),
                    gen_tcp:close(Socket),
                    maybe_verify_download(Result, FileName, Ip, Port);
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

% Lee la cabecera inicial y decide si el archivo existe o no.
receive_file(Socket, FileName) ->
    case gen_tcp:recv(Socket, 1, ?TCP_HEADER_TIMEOUT) of
        {ok, <<?CODE_OK>>} ->
            case gen_tcp:recv(Socket, 4, ?TCP_HEADER_TIMEOUT) of
                {ok, SizeBin} ->
                    <<Size:32/integer-big>> = SizeBin,
                    receive_file_payload(Socket, FileName, Size);
                {error, closed} ->
                    io:format("Conexion cerrada~n"),
                    {error, connection_closed};
                {error, Reason} ->
                    io:format("Error: ~p~n", [Reason]),
                    {error, Reason}
            end;
        {ok, <<?CODE_NOTFOUND>>} ->
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

% La descarga simple sigue el protocolo base del enunciado.
receive_file_payload(Socket, FileName, Size) ->
    if
        Size > ?LARGE_FILE_THRESHOLD ->
            case gen_tcp:recv(Socket, 4, ?TCP_HEADER_TIMEOUT) of
                {ok, <<_TransferChunkSize:32/integer-big>>} ->
                    receive_chunked_file(Socket, FileName);
                {error, ChunkReason} ->
                    io:format("Error metadata: ~p~n", [ChunkReason]),
                    {error, chunk_metadata_failed}
            end;
        true ->
            receive_small_file(Socket, FileName, Size)
    end.

% Recibe un archivo chico de una sola vez y lo guarda en descargas.
receive_small_file(Socket, FileName, Size) ->
    case gen_tcp:recv(Socket, Size, ?CHUNK_TIMEOUT) of
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
    end.

% Prepara el archivo final y empieza a recibir la secuencia de chunks.
receive_chunked_file(Socket, FileName) ->
    FilePath = filename:join(?DOWNLOAD_DIR, FileName),
    file:delete(FilePath),
    case receive_chunks_loop(Socket, FilePath) of
        {ok, _} ->
            io:format("Descarga completa: ~s~n", [FileName]),
            {ok, FileName};
        Error ->
            Error
    end.

% Recibe todos los chunks del protocolo base hasta que el socket cierre.
receive_chunks_loop(Socket, FilePath) ->
    case gen_tcp:recv(Socket, 1, ?TCP_HEADER_TIMEOUT) of
        {ok, <<?CODE_CHUNK>>} ->
            case gen_tcp:recv(Socket, 2, ?TCP_HEADER_TIMEOUT) of
                {ok, _IndexBin} ->
                    case gen_tcp:recv(Socket, 2, ?TCP_HEADER_TIMEOUT) of
                        {ok, ChunkSizeBin} ->
                            <<ChunkSize:16/integer-big>> = ChunkSizeBin,
                            case gen_tcp:recv(Socket, ChunkSize, ?CHUNK_TIMEOUT) of
                                {ok, ChunkData} ->
                                    % Cada chunk se agrega al archivo destino en orden de llegada.
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

% Guarda un archivo completo en la carpeta de descargas.
save_file(FileName, Data) ->
    filelib:ensure_dir(?DOWNLOAD_DIR ++ "/"),
    FilePath = filename:join(?DOWNLOAD_DIR, FileName),
    case file:write_file(FilePath, Data) of
        ok -> ok;
        {error, Reason} -> io:format("Error guardando: ~p~n", [Reason])
    end.

% La descarga multi-fuente usa una extension minima del protocolo:
% cada worker pide un chunk puntual con DOWNLOAD_CHUNK.
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

% Consulta a un nodo remoto si tiene el archivo buscado.
search_in_node(Parent, MyNodeId, Ip, Port, FileName) ->
    case gen_tcp:connect(Ip, Port, [binary, {active, false}, {reuseaddr, true}], ?SEARCH_TIMEOUT) of
        {ok, Socket} ->
            Request = io_lib:format("SEARCH_REQUEST ~s ~s~n", [MyNodeId, FileName]),
            gen_tcp:send(Socket, Request),
            Results = receive_search_responses(Socket, FileName, []),
            gen_tcp:close(Socket),
            Parent ! {search_result, Results};
        {error, _Reason} ->
            Parent ! {search_result, []}
    end.

% Reune todas las respuestas SEARCH_RESPONSE de un mismo nodo.
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

% Toma solo las respuestas que coinciden con el archivo pedido.
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

% Interpreta el estado de chunks informado por la busqueda.
parse_chunk_status("COMPLETE") ->
    complete;
parse_chunk_status("CHUNKS:" ++ ChunkList) ->
    ChunkIds = [list_to_integer(C) || C <- string:tokens(ChunkList, ",")],
    {partial, ChunkIds};
parse_chunk_status(_) ->
    complete.

% Junta respuestas de varios nodos hasta completar o vencer el timeout.
collect_search_results(0, Results) ->
    Results;
collect_search_results(Remaining, Results) ->
    receive
        {search_result, NewResults} ->
            collect_search_results(Remaining - 1, Results ++ NewResults)
    after 3000 ->
        Results
    end.

% Inicializa el estado compartido y lanza los workers por cada fuente.
start_multi_download(FileName, SearchResults) ->
    [{_, _, TotalSize, _} | _] = SearchResults,
    ChunkSize = ?CHUNK_SIZE,
    download_manager:start(),
    {ok, _State} = download_manager:init_download(FileName, TotalSize, ChunkSize, SearchResults),

    SourceNodes = extract_source_nodes(SearchResults),
    WorkersPerNode = ?WORKERS_PER_NODE,
    TotalWorkers = length(SourceNodes) * WorkersPerNode,

    io:format("Iniciando descarga paralela (~p nodos, ~p workers)~n", [length(SourceNodes), TotalWorkers]),

    Parent = self(),
    lists:foreach(fun(NodeId) ->
        lists:foreach(fun(_) ->
            spawn(fun() -> download_worker(Parent, FileName, NodeId) end)
        end, lists:seq(1, WorkersPerNode))
    end, SourceNodes),

    wait_for_completion(FileName, TotalWorkers, SourceNodes).

% Extrae la lista unica de nodos que pueden servir el archivo.
extract_source_nodes(SearchResults) ->
    lists:usort([NodeId || {NodeId, _, _, _} <- SearchResults]).

% Pide trabajo al coordinador y descarga chunks hasta quedarse sin tareas.
download_worker(Parent, FileName, NodeId) ->
    case download_manager:assign_chunk(FileName, NodeId) of
        {ok, ChunkId} ->
            case download_chunk_from_node(FileName, ChunkId, NodeId) of
                ok ->
                    download_manager:mark_chunk_complete(FileName, ChunkId),
                    download_worker(Parent, FileName, NodeId);
                {error, Reason} ->
                    % El chunk fallido vuelve a pending para que se pueda reintentar.
                    download_manager:mark_chunk_failed(FileName, ChunkId),
                    io:format("Error descargando chunk ~p desde ~s: ~p~n", [ChunkId, NodeId, Reason]),
                    Parent ! {worker_error, NodeId, ChunkId},
                    % Si la fuente cayo de verdad, este worker ya no sigue insistiendo.
                    case should_stop_worker(Reason) of
                        true ->
                            io:format("Worker ~s detenido: nodo no disponible~n", [NodeId]),
                            Parent ! {worker_done, NodeId};
                        false ->
                            download_worker(Parent, FileName, NodeId)
                    end
            end;
        {error, no_chunks} ->
            Parent ! {worker_done, NodeId}
    end.

% Esta rama se usa solamente en la descarga multi-fuente.
download_chunk_from_node(FileName, ChunkId, NodeId) ->
    case node_registry:get_node(NodeId) of
        {ok, {_Id, Ip, Port}} ->
            case gen_tcp:connect(Ip, Port, [binary, {active, false}, {reuseaddr, true}], ?TCP_CONNECT_TIMEOUT) of
                {ok, Socket} ->
                    Request = io_lib:format("DOWNLOAD_CHUNK ~s ~p~n", [FileName, ChunkId]),
                    case gen_tcp:send(Socket, Request) of
                        ok ->
                            Result = receive_requested_chunk(Socket, FileName, ChunkId),
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

% Recibe un chunk puntual pedido con la extension DOWNLOAD_CHUNK.
receive_requested_chunk(Socket, FileName, ChunkId) ->
    case gen_tcp:recv(Socket, 1, ?TCP_HEADER_TIMEOUT) of
        {ok, <<?CODE_OK>>} ->
            case gen_tcp:recv(Socket, 4, ?TCP_HEADER_TIMEOUT) of
                {ok, SizeBin} ->
                    <<Size:32/integer-big>> = SizeBin,
                    case gen_tcp:recv(Socket, Size, ?CHUNK_TIMEOUT) of
                        {ok, ChunkData} ->
                            save_chunk(FileName, ChunkId, ChunkData),
                            ok;
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        {ok, <<?CODE_NOTFOUND>>} ->
            {error, file_not_found};
        {ok, Other} ->
            io:format("Respuesta invalida para chunk ~p: ~p~n", [ChunkId, Other]),
            {error, invalid_chunk_response};
        {error, Reason} ->
            {error, Reason}
    end.

% Guarda cada chunk en disco para poder ensamblarlo al final.
save_chunk(FileName, ChunkId, Data) ->
    ChunkDir = filename:join(?DOWNLOAD_DIR, "chunks"),
    filelib:ensure_dir(ChunkDir ++ "/"),
    ChunkPath = filename:join(ChunkDir, io_lib:format("~s.chunk~p", [FileName, ChunkId])),
    file:write_file(ChunkPath, Data).

% Espera a que terminen los workers o a que la descarga quede completa.
wait_for_completion(FileName, RemainingWorkers, SourceNodes) ->
    case download_manager:is_complete(FileName) of
        {ok, true} ->
            io:format("~nTodos los chunks descargados, ensamblando...~n"),
            case assemble_file(FileName) of
                ok ->
                    verify_assembled_file(FileName, SourceNodes),
                    download_manager:stop();
                {error, Reason} ->
                    io:format("Error ensamblando archivo: ~p~n", [Reason]),
                    download_manager:stop()
            end;
        {ok, false} ->
            receive
                {worker_done, NodeId} ->
                    io:format("Worker ~s terminado~n", [NodeId]),
                    wait_for_completion(FileName, RemainingWorkers - 1, SourceNodes);
                {worker_error, NodeId, ChunkId} ->
                    % El reintento real lo hace otro pedido al coordinador.
                    io:format("Error en worker ~s chunk ~p, reintentando...~n", [NodeId, ChunkId]),
                    wait_for_completion(FileName, RemainingWorkers, SourceNodes)
            after ?DOWNLOAD_TIMEOUT ->
                io:format("Timeout esperando descarga~n"),
                download_manager:stop()
            end
    end.

% Une todos los chunks descargados y arma el archivo final.
assemble_file(FileName) ->
    ChunkDir = filename:join(?DOWNLOAD_DIR, "chunks"),
    Pattern = filename:join(ChunkDir, FileName ++ ".chunk*"),
    ChunkFiles = filelib:wildcard(Pattern),

    ChunkData = lists:sort(lists:filtermap(fun(File) ->
        case parse_chunk_id(File) of
            {ok, Id} ->
                {ok, Data} = file:read_file(File),
                {true, {Id, Data}};
            error ->
                false
        end
    end, ChunkFiles)),

    FinalPath = filename:join(?DOWNLOAD_DIR, FileName),
    {ok, OutFile} = file:open(FinalPath, [write, binary]),

    lists:foreach(fun({_Id, Data}) ->
        file:write(OutFile, Data)
    end, ChunkData),

    file:close(OutFile),

    lists:foreach(fun(ChunkFile) ->
        file:delete(ChunkFile)
    end, ChunkFiles),

    io:format("Archivo ensamblado: ~s~n", [FileName]),
    ok.

% Lee el archivo ensamblado y dispara la verificacion de integridad.
verify_assembled_file(FileName, SourceNodes) ->
    FinalPath = filename:join(?DOWNLOAD_DIR, FileName),
    case file:read_file(FinalPath) of
        {ok, Data} ->
            io:format("Archivo descargado: ~s~n", [FileName]),
            verify_file_data(FileName, Data, SourceNodes);
        {error, Reason} ->
            io:format("Error verificando archivo: ~p~n", [Reason])
    end.

% Verifica la descarga simple si efectivamente termino bien.
maybe_verify_download({ok, FileName}, FileName, Ip, Port) ->
    FilePath = filename:join(?DOWNLOAD_DIR, FileName),
    case file:read_file(FilePath) of
        {ok, Data} ->
            verify_file_data(FileName, Data, [{Ip, Port}]);
        {error, Reason} ->
            io:format("Error verificando archivo: ~p~n", [Reason]),
            {error, Reason}
    end;
maybe_verify_download(Result, _FileName, _Ip, _Port) ->
    Result.

% Compara el hash local con el hash informado por algun nodo fuente.
verify_file_data(FileName, Data, Sources) ->
    LocalHash = binary_to_hex(crypto:hash(sha256, Data)),
    case get_expected_checksum(FileName, Sources) of
        {ok, RemoteHash} ->
            case LocalHash =:= RemoteHash of
                true ->
                    io:format("Descarga completada [VERIFICADO]~n"),
                    {ok, FileName};
                false ->
                    io:format("ADVERTENCIA: checksum no coincide para ~s~n", [FileName]),
                    io:format("  Esperado: ~s~n", [RemoteHash]),
                    io:format("  Obtenido: ~s~n", [LocalHash]),
                    {error, checksum_mismatch}
            end;
        {error, not_supported} ->
            io:format("Descarga completada~n"),
            io:format("No se pudo verificar integridad con el nodo origen~n"),
            {ok, FileName};
        {error, Reason} ->
            io:format("Descarga completada~n"),
            io:format("No se pudo verificar integridad: ~p~n", [Reason]),
            {ok, FileName}
    end.

% Va probando distintas fuentes hasta conseguir un checksum valido.
get_expected_checksum(_FileName, []) ->
    {error, no_sources};
get_expected_checksum(FileName, [{Ip, Port} | Rest]) when is_tuple(Ip) ->
    case request_remote_checksum(Ip, Port, FileName) of
        {ok, Hash} -> {ok, Hash};
        _ -> get_expected_checksum(FileName, Rest)
    end;
get_expected_checksum(FileName, [NodeId | Rest]) ->
    case node_registry:get_node(NodeId) of
        {ok, {_Id, Ip, Port}} ->
            case request_remote_checksum(Ip, Port, FileName) of
                {ok, Hash} -> {ok, Hash};
                _ -> get_expected_checksum(FileName, Rest)
            end;
        _ ->
            get_expected_checksum(FileName, Rest)
    end.

% Pide el checksum remoto con una extension separada del flujo de descarga.
request_remote_checksum(Ip, Port, FileName) ->
    case gen_tcp:connect(Ip, Port, [binary, {active, false}, {reuseaddr, true}], ?TCP_CONNECT_TIMEOUT) of
        {ok, Socket} ->
            Request = io_lib:format("CHECKSUM_REQUEST ~s~n", [FileName]),
            case gen_tcp:send(Socket, Request) of
                ok ->
                    Response = gen_tcp:recv(Socket, 0, ?SEARCH_TIMEOUT),
                    gen_tcp:close(Socket),
                    parse_checksum_response(Response);
                {error, Reason} ->
                    gen_tcp:close(Socket),
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

% Interpreta la respuesta del pedido de checksum.
parse_checksum_response({ok, Data}) ->
    Tokens = string:tokens(string:trim(binary_to_list(Data)), " "),
    case Tokens of
        ["CHECKSUM_RESPONSE", Hash] ->
            {ok, Hash};
        ["CHECKSUM_NOTFOUND"] ->
            {error, not_found};
        _ ->
            {error, not_supported}
    end;
parse_checksum_response({error, Reason}) ->
    {error, Reason}.

% Convierte el hash binario a texto hexadecimal para poder compararlo.
binary_to_hex(Bin) ->
    lists:flatten([io_lib:format("~2.16.0B", [Byte]) || <<Byte>> <= Bin]).

% Decide cuando conviene dejar de usar una fuente caida o no disponible.
should_stop_worker(node_not_found) ->
    true;
should_stop_worker(connection_refused) ->
    true;
should_stop_worker(connection_closed) ->
    true;
should_stop_worker(connection_closed_during_chunk) ->
    true;
should_stop_worker(connection_timeout) ->
    true;
should_stop_worker(econnrefused) ->
    true;
should_stop_worker(_) ->
    false.

% Extrae el indice del chunk a partir del nombre de archivo temporal.
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

% Lee el ID del nodo desde el coordinador principal.
get_node_id() ->
    case whereis(p2p_node) of
        undefined ->
            {ok, "unknown"};
        Pid ->
            case process_info(Pid, dictionary) of
                {dictionary, Dict} ->
                    case proplists:get_value(node_id, Dict) of
                        undefined -> {ok, "unknown"};
                        NodeId -> {ok, NodeId}
                    end;
                _ ->
                    {ok, "unknown"}
            end
    end.
