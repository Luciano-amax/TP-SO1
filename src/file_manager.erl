-module(file_manager).
-include("config.hrl").
-include_lib("kernel/include/file.hrl").
-export([start/0, stop/0, get_shared_files/0, search_files/1, get_file/1, get_available_chunks/1]).


% Arranca el proceso que maneja los archivos
start() ->
    Pid = spawn(fun() -> init() end),
    register(file_manager, Pid),
    ok.

stop() ->
    case whereis(file_manager) of
        undefined -> ok;
        Pid -> 
            Pid ! stop,
            unregister(file_manager),
            ok
    end.

% Devuelve la lista de archivos que tenemos para compartir
get_shared_files() ->
    file_manager ! {get_files, self()},
    receive
        {files, Files} -> Files
    after 5000 ->
        []
    end.

% Busca archivos que coincidan con el patron (soporta wildcards)
search_files(Pattern) ->
    SharedDir = ?SHARED_DIR,
    Files = filelib:wildcard(Pattern, SharedDir),
    lists:filtermap(fun(FileName) ->
        FilePath = filename:join(SharedDir, FileName),
        case filelib:is_regular(FilePath) of
            true ->
                Size = filelib:file_size(FilePath),
                {true, {FileName, Size}};
            false ->
                false
        end
    end, Files).

% Inicializa escaneando la carpeta compartida
init() ->
    Files = scan_directory(),
    io:format("Archivos escaneados: ~p~n", [length(Files)]),
    loop(Files).

loop(Files) ->
    receive
        {get_files, From} ->
            From ! {files, Files},
            loop(Files);
        stop ->
            ok
    end.

% Lee un archivo de la carpeta compartida y devuelve su contenido
get_file(FileName) ->
    SharedDir = ?SHARED_DIR,
    FilePath = filename:join(SharedDir, FileName),
    case filelib:is_regular(FilePath) of
        true ->
            case file:read_file(FilePath) of
                {ok, Data} ->
                    Size = byte_size(Data),
                    {ok, Data, Size};
                {error, _Reason} ->
                    {error, not_found}
            end;
        false ->
            {error, not_found}
    end.

% Escanea el directorio y arma la lista de archivos
scan_directory() ->
    SharedDir = ?SHARED_DIR,
    case filelib:is_dir(SharedDir) of
        false ->
            io:format("Directorio ~s no existe~n", [SharedDir]),
            [];
        true ->
            case file:list_dir(SharedDir) of
                {ok, FileNames} ->
                    % Filtramos solo archivos regulares y armamos tuplas con toda la info
                    lists:filtermap(fun(FileName) ->
                        FilePath = filename:join(SharedDir, FileName),
                        case filelib:is_regular(FilePath) of
                            true ->
                                case file:read_file_info(FilePath) of
                                    {ok, FileInfo} ->
                                        Size = FileInfo#file_info.size,
                                        % Devolvemos {Nombre, Tamaño, Path}
                                        {true, {FileName, Size, FilePath}};
                                    _ ->
                                        false
                                end;
                            false ->
                                % Si no es un archivo regular (ej: directorio), lo ignoramos
                                false
                        end
                    end, FileNames);
                {error, _} ->
                    []
            end
    end.

% Detecta que chunks tiene el nodo para un archivo
get_available_chunks(FileName) ->
    CompletePath = filename:join(?SHARED_DIR, FileName),
    case filelib:is_regular(CompletePath) of
        true ->
            Size = filelib:file_size(CompletePath),
            TotalChunks = calculate_total_chunks(Size, 4194304),
            {complete, TotalChunks};
        false ->
            ChunkDir = filename:join(?DOWNLOAD_DIR, "chunks"),
            case find_chunk_files(ChunkDir, FileName) of
                [] -> 
                    not_found;
                ChunkIds ->
                    {partial, lists:sort(ChunkIds)}
            end
    end.

% Busca archivos de chunks en disco
find_chunk_files(Dir, FileName) ->
    Pattern = filename:join(Dir, FileName ++ ".chunk*"),
    Files = filelib:wildcard(Pattern),
    lists:filtermap(fun(File) ->
        case parse_chunk_id(File) of
            {ok, Id} -> {true, Id};
            error -> false
        end
    end, Files).

% Extrae el ID del chunk del nombre de archivo
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

% Calcula cantidad total de chunks
calculate_total_chunks(TotalSize, ChunkSize) ->
    (TotalSize + ChunkSize - 1) div ChunkSize.
