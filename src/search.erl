-module(search).
-export([search_all_nodes/1]).
-include("config.hrl").

% Busca un patrón en todos los nodos conocidos
search_all_nodes(Pattern) ->
    {ok, MyNodeId} = get_node_id(),
    Nodes = node_registry:get_all_nodes(),
    
    io:format("~nBuscando '~s' en ~p nodo(s)...~n", [Pattern, length(Nodes)]),
    
    Parent = self(),
    lists:foreach(fun({NodeId, Ip, Port}) ->
        spawn(fun() -> search_in_node(Parent, MyNodeId, NodeId, Ip, Port, Pattern) end)
    end, Nodes),
    
    collect_results(length(Nodes), []).

% Busca en un nodo específico
search_in_node(Parent, MyNodeId, NodeId, Ip, Port, Pattern) ->
    case gen_tcp:connect(Ip, Port, [binary, {active, false}, {reuseaddr, true}], 2000) of
        {ok, Socket} ->
            Request = io_lib:format("SEARCH_REQUEST ~s ~s~n", [MyNodeId, Pattern]),
            gen_tcp:send(Socket, Request),
            
            Results = receive_all_responses(Socket, NodeId, []),
            gen_tcp:close(Socket),
            Parent ! {search_result, Results};
        {error, _Reason} ->
            Parent ! {search_result, []}
    end.

% Recibe todas las respuestas de un nodo
receive_all_responses(Socket, NodeId, Acc) ->
    case gen_tcp:recv(Socket, 0, 1000) of
        {ok, Data} ->
            Lines = string:tokens(binary_to_list(Data), "\n"),
            NewResults = lists:filtermap(fun(Line) ->
                parse_search_response(Line)
            end, Lines),
            receive_all_responses(Socket, NodeId, Acc ++ NewResults);
        {error, _} ->
            Acc
    end.

% Parsea una respuesta SEARCH_RESPONSE
parse_search_response(Line) ->
    Tokens = string:tokens(string:trim(Line), " "),
    case Tokens of
        ["SEARCH_RESPONSE", NodeId, FileName, SizeStr] ->
            {Size, _} = string:to_integer(SizeStr),
            {true, {NodeId, FileName, Size}};
        _ ->
            false
    end.

% Recolecta resultados de todos los nodos
collect_results(0, Results) ->
    display_results(Results);
collect_results(Remaining, Results) ->
    receive
        {search_result, NewResults} ->
            collect_results(Remaining - 1, Results ++ NewResults)
    after 3000 ->
        display_results(Results)
    end.

% Muestra los resultados
display_results([]) ->
    io:format("~nNo se encontraron archivos.~n~n");
display_results(Results) ->
    io:format("~n=== Resultados de búsqueda ===~n"),
    lists:foreach(fun({NodeId, FileName, Size}) ->
        SizeMB = Size / (1024 * 1024),
        io:format("  [~s] ~s (~.2f MB)~n", [NodeId, FileName, SizeMB])
    end, Results),
    io:format("~nTotal: ~p archivo(s) encontrado(s)~n~n", [length(Results)]).

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
