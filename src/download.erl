-module(download).
-include("config.hrl").
-export([download_from_node/2]).

% Descarga un archivo desde un nodo específico
download_from_node(FileName, NodeId) ->
    case node_registry:get_node(NodeId) of
        {ok, {_Id, Ip, Port}} ->
            io:format("Descargando ~s desde ~s (~p:~p)...~n", [FileName, NodeId, Ip, Port]),
            connect_and_download(Ip, Port, FileName);
        {error, not_found} ->
            io:format("Error: Nodo ~s no encontrado~n", [NodeId])
    end.

% Conecta al nodo y descarga el archivo
connect_and_download(Ip, Port, FileName) ->
    case gen_tcp:connect(Ip, Port, [binary, {active, false}, {reuseaddr, true}], 5000) of
        {ok, Socket} ->
            Request = io_lib:format("DOWNLOAD_REQUEST ~s~n", [FileName]),
            gen_tcp:send(Socket, Request),
            receive_file(Socket, FileName),
            gen_tcp:close(Socket);
        {error, Reason} ->
            io:format("Error de conexion: ~p~n", [Reason])
    end.

% Recibe el archivo del servidor
receive_file(Socket, FileName) ->
    case gen_tcp:recv(Socket, 1, 5000) of
        {ok, <<101>>} ->
            % Código OK, el archivo existe
            case gen_tcp:recv(Socket, 4, 5000) of
                {ok, SizeBin} ->
                    <<Size:32/integer-big>> = SizeBin,
                    MB = 1024 * 1024,
                    
                    if
                        Size > (MB * 4) ->
                            % Archivo grande, recibir en chunks
                            {ok, <<_ChunkSize:32/integer-big>>} = gen_tcp:recv(Socket, 4, 5000),
                            receive_chunks(Socket, FileName);
                        true ->
                            % Archivo pequeño, recibir todo de una vez
                            case gen_tcp:recv(Socket, Size, 30000) of
                                {ok, Data} ->
                                    save_file(FileName, Data),
                                    io:format("Descarga completada: ~s~n", [FileName]);
                                {error, Reason} ->
                                    io:format("Error al recibir archivo: ~p~n", [Reason])
                            end
                    end;
                {error, Reason} ->
                    io:format("Error al recibir tamaño: ~p~n", [Reason])
            end;
        {ok, <<112>>} ->
            io:format("Error: Archivo no disponible en el nodo remoto~n");
        {ok, Other} ->
            io:format("Respuesta incorrecta del servidor: ~p~n", [Other]);
        {error, Reason} ->
            io:format("Error al recibir respuesta: ~p~n", [Reason])
    end.

% Recibe archivo en chunks
receive_chunks(Socket, FileName) ->
    FilePath = filename:join(?DOWNLOAD_DIR, FileName),
    receive_chunks_loop(Socket, FilePath).

receive_chunks_loop(Socket, FilePath) ->
    case gen_tcp:recv(Socket, 1, 5000) of
        {ok, <<111>>} ->
            % Chunk header
            case gen_tcp:recv(Socket, 2, 5000) of
                {ok, _IndexBin} ->
                    % Índice del chunk (no lo usamos)
                    case gen_tcp:recv(Socket, 4, 5000) of
                        {ok, ChunkSizeBin} ->
                            <<ChunkSize:32/integer-big>> = ChunkSizeBin,
                            case gen_tcp:recv(Socket, ChunkSize, 30000) of
                                {ok, ChunkData} ->
                                    file:write_file(FilePath, ChunkData, [append]),
                                    receive_chunks_loop(Socket, FilePath);
                                {error, _Reason} ->
                                    io:format("Descarga completada: ~s~n", [filename:basename(FilePath)])
                            end;
                        {error, _} ->
                            io:format("Descarga completada: ~s~n", [filename:basename(FilePath)])
                    end;
                {error, _} ->
                    io:format("Descarga completada: ~s~n", [filename:basename(FilePath)])
            end;
        {ok, _Other} ->
            io:format("Error en formato de chunk~n");
        {error, timeout} ->
            io:format("Descarga completada: ~s~n", [filename:basename(FilePath)]);
        {error, _Reason} ->
            io:format("Descarga completada: ~s~n", [filename:basename(FilePath)])
    end.

% Guarda el archivo en el directorio de descargas
save_file(FileName, Data) ->
    filelib:ensure_dir(?DOWNLOAD_DIR ++ "/"),
    FilePath = filename:join(?DOWNLOAD_DIR, FileName),
    case file:write_file(FilePath, Data) of
        ok ->
            ok;
        {error, Reason} ->
            io:format("Error al guardar archivo: ~p~n", [Reason])
    end.
