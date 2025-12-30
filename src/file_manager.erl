% Gestor de archivos compartidos
-module(file_manager).
-include("config.hrl").
-export([start/0, stop/0, get_shared_files/0]).

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

% Escanea el directorio y arma la lista de archivos
scan_directory() ->
    case filelib:is_dir(?SHARED_DIR) of
        false ->
            io:format("Directorio ~s no existe~n", [?SHARED_DIR]),
            [];
        true ->
            case file:list_dir(?SHARED_DIR) of
                {ok, FileNames} ->
                    % Filtramos solo archivos regulares y armamos tuplas con toda la info
                    lists:filtermap(fun(FileName) ->
                        FilePath = filename:join(?SHARED_DIR, FileName),
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
