-module(search).
-export([search_all_nodes/1]).
-include("config.hrl").

% Hace la busqueda local y remota, y despues junta todos los resultados.
search_all_nodes(Pattern) ->
    {ok, MyNodeId} = get_node_id(),
    Nodes = node_registry:get_all_nodes(),
    % Se incorporan primero los resultados locales 
    LocalResults = get_local_results(MyNodeId, Pattern),
    
    io:format("~nBuscando '~s'...~n", [Pattern]),
    
    Parent = self(),
    lists:foreach(fun({_NodeId, Ip, Port}) ->
        spawn(fun() -> search_in_node(Parent, MyNodeId, Ip, Port, Pattern) end)
    end, Nodes),
    
    collect_results(length(Nodes), LocalResults).

% Busca en un nodo especifico
search_in_node(Parent, MyNodeId, Ip, Port, Pattern) ->
    case gen_tcp:connect(Ip, Port, [binary, {active, false}, {reuseaddr, true}], 2000) of
        {ok, Socket} ->
            Request = io_lib:format("SEARCH_REQUEST ~s ~s~n", [MyNodeId, Pattern]),
            gen_tcp:send(Socket, Request),
            
            % Un mismo nodo puede responder varias lineas si encuentra varios archivos.
            Results = receive_all_responses(Socket, []),
            gen_tcp:close(Socket),
            Parent ! {search_result, Results};
        {error, _Reason} ->
            Parent ! {search_result, []}
    end.

% Recibe todas las respuestas de un nodo
receive_all_responses(Socket, Acc) ->
    case gen_tcp:recv(Socket, 0, 1000) of
        {ok, Data} ->
            Lines = string:tokens(binary_to_list(Data), "\n"),
            NewResults = lists:filtermap(fun(Line) ->
                parse_search_response(Line)
            end, Lines),
            receive_all_responses(Socket, Acc ++ NewResults);
        {error, _} ->
            Acc
    end.

% Parsea una respuesta SEARCH_RESPONSE
parse_search_response(Line) ->
    Tokens = string:tokens(string:trim(Line), " "),
    case Tokens of
        ["SEARCH_RESPONSE", NodeId, FileName, SizeStr, _HashHex, _ChunkCountStr, BitfieldStr] ->
            case string:to_integer(SizeStr) of
                {Size, _} when is_integer(Size) ->
                    ChunkInfo = parse_bitfield_status(BitfieldStr),
                    {true, {NodeId, FileName, Size, ChunkInfo}};
                _ ->
                    false
            end;
        ["SEARCH_RESPONSE", NodeId, FileName, SizeStr, Status | _Rest] ->
            case string:to_integer(SizeStr) of
                {Size, _} when is_integer(Size) ->
                    ChunkInfo = parse_chunk_status(Status),
                    {true, {NodeId, FileName, Size, ChunkInfo}};
                _ ->
                    false
            end;
        ["SEARCH_RESPONSE", NodeId, FileName, SizeStr] ->
            % Compatibilidad con formato viejo
            case string:to_integer(SizeStr) of
                {Size, _} when is_integer(Size) ->
                    {true, {NodeId, FileName, Size, complete}};
                _ ->
                    false
            end;
        _ ->
            false
    end.

% Parsea el estado de chunks: "COMPLETE" o "CHUNKS:0,2,4"
parse_chunk_status("COMPLETE") ->
    complete;
parse_chunk_status("CHUNKS:" ++ ChunkList) ->
    ChunkIds = [list_to_integer(C) || C <- string:tokens(ChunkList, ",")],
    {partial, ChunkIds};
parse_chunk_status(_) ->
    complete.

parse_bitfield_status(BitfieldStr) ->
    IndexedBits = lists:zip(lists:seq(0, length(BitfieldStr) - 1), BitfieldStr),
    ChunkIds = [Idx || {Idx, Bit} <- IndexedBits, Bit =:= $1],
    case ChunkIds of
        [] ->
            complete;
        _ when length(ChunkIds) =:= length(BitfieldStr) ->
            complete;
        _ ->
            {partial, ChunkIds}
    end.

% Convierte los resultados propios al mismo formato que usan los remotos.
get_local_results(MyNodeId, Pattern) ->
    Files = file_manager:search_files(Pattern),
    % Los archivos propios se informan como completos porque forman
    % parte de la carpeta compartida del nodo local.
    [{MyNodeId, FileName, Size, complete} || {FileName, Size} <- Files].

% Espera respuestas de todos los nodos o corta por timeout.
collect_results(0, Results) ->
    display_results(Results);
collect_results(Remaining, Results) ->
    receive
        {search_result, NewResults} ->
            collect_results(Remaining - 1, Results ++ NewResults)
    after 3000 ->
        display_results(Results)
    end.

% Muestra los resultados finales de forma unificada.
display_results([]) ->
    io:format("~nSin resultados.~n~n");
display_results(Results) ->
    io:format("~nResultados:~n"),
    lists:foreach(fun(Result) ->
        case Result of
            {NodeId, FileName, Size, complete} ->
                SizeMB = Size / (1024 * 1024),
                io:format("  [~s] ~s (~.2f MB) [COMPLETO]~n", [NodeId, FileName, SizeMB]);
            {NodeId, FileName, Size, {partial, ChunkIds}} ->
                SizeMB = Size / (1024 * 1024),
                ChunkStr = string:join([integer_to_list(C) || C <- ChunkIds], ","),
                io:format("  [~s] ~s (~.2f MB) [PARCIAL: chunks ~s]~n", 
                         [NodeId, FileName, SizeMB, ChunkStr]);
            {NodeId, FileName, Size} ->
                % Compatibilidad con formato viejo
                SizeMB = Size / (1024 * 1024),
                io:format("  [~s] ~s (~.2f MB)~n", [NodeId, FileName, SizeMB])
        end
    end, Results),
    io:format("~n").

% Obtiene el NodeId actual
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
