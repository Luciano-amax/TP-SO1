-module(download_manager).
-export([start/0, stop/0, init_download/4, mark_chunk_complete/2, 
         get_progress/1, get_missing_chunks/1, get_chunk_sources/2,
         assign_chunk/2, is_complete/1, get_download_state/1]).

-include("config.hrl").

% Estado de una descarga multi-fuente
-record(download_state, {
    filename,
    total_size,
    chunk_size,
    total_chunks,
    chunks_status,    % #{ChunkId => pending | downloading | complete}
    chunk_sources,    % #{ChunkId => [NodeId]}
    active_downloads  % #{ChunkId => NodeId}
}).

start() ->
    Pid = spawn(fun() -> loop(#{}) end),
    register(download_manager, Pid),
    ok.

stop() ->
    case whereis(download_manager) of
        undefined -> ok;
        Pid ->
            Pid ! stop,
            unregister(download_manager),
            ok
    end.

% Inicializa una nueva descarga con info de nodos y chunks
init_download(FileName, TotalSize, ChunkSize, SearchResults) ->
    download_manager ! {init_download, FileName, TotalSize, ChunkSize, SearchResults, self()},
    receive
        {download_initialized, State} -> {ok, State}
    after 5000 ->
        {error, timeout}
    end.

% Marca un chunk como completado
mark_chunk_complete(FileName, ChunkId) ->
    download_manager ! {mark_complete, FileName, ChunkId},
    ok.

% Asigna un chunk pendiente a un nodo especifico
assign_chunk(FileName, NodeId) ->
    download_manager ! {assign_chunk, FileName, NodeId, self()},
    receive
        {chunk_assigned, ChunkId} -> {ok, ChunkId};
        {no_chunks_available} -> {error, no_chunks}
    after 5000 ->
        {error, timeout}
    end.

% Obtiene el progreso de descarga en porcentaje
get_progress(FileName) ->
    download_manager ! {get_progress, FileName, self()},
    receive
        {progress, Percent} -> {ok, Percent}
    after 5000 ->
        {error, timeout}
    end.

% Obtiene lista de chunks que faltan descargar
get_missing_chunks(FileName) ->
    download_manager ! {get_missing, FileName, self()},
    receive
        {missing_chunks, Chunks} -> {ok, Chunks}
    after 5000 ->
        {error, timeout}
    end.

% Obtiene lista de nodos que tienen un chunk especifico
get_chunk_sources(FileName, ChunkId) ->
    download_manager ! {get_sources, FileName, ChunkId, self()},
    receive
        {chunk_sources, Sources} -> {ok, Sources}
    after 5000 ->
        {error, timeout}
    end.

% Verifica si la descarga esta completa
is_complete(FileName) ->
    download_manager ! {is_complete, FileName, self()},
    receive
        {complete_status, Status} -> {ok, Status}
    after 5000 ->
        {error, timeout}
    end.

% Obtiene el estado completo de una descarga
get_download_state(FileName) ->
    download_manager ! {get_state, FileName, self()},
    receive
        {download_state, State} -> {ok, State};
        {not_found} -> {error, not_found}
    after 5000 ->
        {error, timeout}
    end.

% Loop principal que mantiene el estado de todas las descargas
loop(Downloads) ->
    receive
        {init_download, FileName, TotalSize, ChunkSize, SearchResults, From} ->
            TotalChunks = calculate_total_chunks(TotalSize, ChunkSize),
            ChunkSources = build_chunk_sources(SearchResults, TotalChunks),
            ChunksStatus = maps:from_list([{I, pending} || I <- lists:seq(0, TotalChunks - 1)]),
            
            State = #download_state{
                filename = FileName,
                total_size = TotalSize,
                chunk_size = ChunkSize,
                total_chunks = TotalChunks,
                chunks_status = ChunksStatus,
                chunk_sources = ChunkSources,
                active_downloads = #{}
            },
            
            io:format("Descarga inicializada: ~s (~p chunks)~n", [FileName, TotalChunks]),
            From ! {download_initialized, State},
            loop(maps:put(FileName, State, Downloads));
        
        {mark_complete, FileName, ChunkId} ->
            case maps:find(FileName, Downloads) of
                {ok, State} ->
                    NewChunksStatus = maps:update(ChunkId, complete, State#download_state.chunks_status),
                    NewActiveDownloads = maps:remove(ChunkId, State#download_state.active_downloads),
                    NewState = State#download_state{
                        chunks_status = NewChunksStatus,
                        active_downloads = NewActiveDownloads
                    },
                    
                    Progress = calculate_progress(NewState),
                    io:format("Chunk ~p completo (~.1f%)~n", [ChunkId, Progress]),
                    
                    loop(maps:put(FileName, NewState, Downloads));
                error ->
                    loop(Downloads)
            end;
        
        {assign_chunk, FileName, NodeId, From} ->
            case maps:find(FileName, Downloads) of
                {ok, State} ->
                    case find_available_chunk(State, NodeId) of
                        {ok, ChunkId} ->
                            NewChunksStatus = maps:update(ChunkId, downloading, State#download_state.chunks_status),
                            NewActiveDownloads = maps:put(ChunkId, NodeId, State#download_state.active_downloads),
                            NewState = State#download_state{
                                chunks_status = NewChunksStatus,
                                active_downloads = NewActiveDownloads
                            },
                            From ! {chunk_assigned, ChunkId},
                            loop(maps:put(FileName, NewState, Downloads));
                        error ->
                            From ! {no_chunks_available},
                            loop(Downloads)
                    end;
                error ->
                    From ! {no_chunks_available},
                    loop(Downloads)
            end;
        
        {get_progress, FileName, From} ->
            case maps:find(FileName, Downloads) of
                {ok, State} ->
                    Progress = calculate_progress(State),
                    From ! {progress, Progress};
                error ->
                    From ! {progress, 0.0}
            end,
            loop(Downloads);
        
        {get_missing, FileName, From} ->
            case maps:find(FileName, Downloads) of
                {ok, State} ->
                    Missing = get_missing_chunks_list(State),
                    From ! {missing_chunks, Missing};
                error ->
                    From ! {missing_chunks, []}
            end,
            loop(Downloads);
        
        {get_sources, FileName, ChunkId, From} ->
            case maps:find(FileName, Downloads) of
                {ok, State} ->
                    Sources = maps:get(ChunkId, State#download_state.chunk_sources, []),
                    From ! {chunk_sources, Sources};
                error ->
                    From ! {chunk_sources, []}
            end,
            loop(Downloads);
        
        {is_complete, FileName, From} ->
            case maps:find(FileName, Downloads) of
                {ok, State} ->
                    Complete = all_chunks_complete(State),
                    From ! {complete_status, Complete};
                error ->
                    From ! {complete_status, false}
            end,
            loop(Downloads);
        
        {get_state, FileName, From} ->
            case maps:find(FileName, Downloads) of
                {ok, State} ->
                    From ! {download_state, State};
                error ->
                    From ! {not_found}
            end,
            loop(Downloads);
        
        stop ->
            ok
    end.

% Calcula cantidad total de chunks
calculate_total_chunks(TotalSize, ChunkSize) ->
    (TotalSize + ChunkSize - 1) div ChunkSize.

% Construye mapa de que nodos tienen cada chunk
build_chunk_sources(SearchResults, TotalChunks) ->
    ChunkMap = maps:new(),
    lists:foldl(fun(Result, Acc) ->
        case Result of
            {NodeId, _FileName, _Size, complete} ->
                lists:foldl(fun(ChunkId, AccMap) ->
                    maps:update_with(ChunkId, fun(Nodes) -> [NodeId | Nodes] end, [NodeId], AccMap)
                end, Acc, lists:seq(0, TotalChunks - 1));
            {NodeId, _FileName, _Size, {partial, ChunkIds}} ->
                lists:foldl(fun(ChunkId, AccMap) ->
                    maps:update_with(ChunkId, fun(Nodes) -> [NodeId | Nodes] end, [NodeId], AccMap)
                end, Acc, ChunkIds);
            _ ->
                Acc
        end
    end, ChunkMap, SearchResults).

% Busca un chunk pendiente que el nodo especificado tenga disponible
find_available_chunk(State, NodeId) ->
    AvailableSources = maps:get(NodeId, invert_chunk_sources(State#download_state.chunk_sources), []),
    PendingChunks = [ChunkId || {ChunkId, Status} <- maps:to_list(State#download_state.chunks_status),
                                 Status =:= pending,
                                 lists:member(ChunkId, AvailableSources)],
    case PendingChunks of
        [ChunkId | _] -> {ok, ChunkId};
        [] -> error
    end.

% Invierte el mapa de chunk_sources para obtener chunks por nodo
invert_chunk_sources(ChunkSources) ->
    maps:fold(fun(ChunkId, Nodes, Acc) ->
        lists:foldl(fun(NodeId, AccMap) ->
            maps:update_with(NodeId, fun(Chunks) -> [ChunkId | Chunks] end, [ChunkId], AccMap)
        end, Acc, Nodes)
    end, #{}, ChunkSources).

% Calcula porcentaje de progreso de la descarga
calculate_progress(State) ->
    Complete = length([1 || {_ChunkId, Status} <- maps:to_list(State#download_state.chunks_status),
                            Status =:= complete]),
    (Complete / State#download_state.total_chunks) * 100.

% Obtiene lista de chunks que no estan completos
get_missing_chunks_list(State) ->
    [ChunkId || {ChunkId, Status} <- maps:to_list(State#download_state.chunks_status),
                Status =/= complete].

% Verifica si todos los chunks estan completos
all_chunks_complete(State) ->
    lists:all(fun({_ChunkId, Status}) -> Status =:= complete end,
              maps:to_list(State#download_state.chunks_status)).
