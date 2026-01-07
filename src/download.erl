-module(download).
-include("config.hrl").
-export([download_from_node/2]).

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
                    
                    case gen_tcp:recv(Socket, 32, 5000) of
                        {ok, ExpectedHash} ->
                            MB = 1024 * 1024,
                            
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
                        {error, HashReason} ->
                            io:format("Error al recibir hash: ~p~n", [HashReason]),
                            {error, hash_receive_failed}
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
