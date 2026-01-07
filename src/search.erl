-module(search).
-export([search_all_nodes/1]).
-include("config.hrl").

search_all_nodes(Pattern) ->
    {ok, MyNodeId} = get_node_id(),
    Nodes = node_registry:get_all_nodes(),
    
    io:format("~nBuscando '~s'...~n", [Pattern]),
    
    Parent = self(),
    lists:foreach(fun({_NodeId, Ip, Port}) ->
        spawn(fun() -> search_in_node(Parent, MyNodeId, Ip, Port, Pattern) end)
    end, Nodes),
    
    collect_results(length(Nodes), []).

% Busca en un nodo especifico
search_in_node(Parent, MyNodeId, Ip, Port, Pattern) ->
    case gen_tcp:connect(Ip, Port, [binary, {active, false}, {reuseaddr, true}], 2000) of
        {ok, Socket} ->
            Request = io_lib:format("SEARCH_REQUEST ~s ~s~n", [MyNodeId, Pattern]),
            gen_tcp:send(Socket, Request),
            
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
        ["SEARCH_RESPONSE", NodeId, FileName, SizeStr, Status | _Rest] ->
            {Size, _} = string:to_integer(SizeStr),
            ChunkInfo = parse_chunk_status(Status),
            {true, {NodeId, FileName, Size, ChunkInfo}};
        ["SEARCH_RESPONSE", NodeId, FileName, SizeStr] ->
            % Compatibilidad con formato viejo
            {Size, _} = string:to_integer(SizeStr),
            {true, {NodeId, FileName, Size, complete}};
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

collect_results(0, Results) ->
    display_results(Results);
collect_results(Remaining, Results) ->
    receive
        {search_result, NewResults} ->
            collect_results(Remaining - 1, Results ++ NewResults)
    after 3000 ->
        display_results(Results)
    end.

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
