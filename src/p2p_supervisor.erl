-module(p2p_supervisor).
-behaviour(supervisor).
-export([start_link/0, init/1]).

% Supervisor para los procesos principales del sistema P2P
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    % Estrategia: one_for_one - si un proceso falla, solo se reinicia ese proceso
    % Intensidad: 5 reinicios en 60 segundos antes de rendirse
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 60
    },
    
    % Especificaciones de los procesos supervisados
    ChildSpecs = [
        % Node Registry - proceso crítico
        #{
            id => node_registry,
            start => {node_registry, start, []},
            restart => permanent,  % Siempre reiniciar si falla
            shutdown => 5000,
            type => worker
        },
        
        % File Manager - proceso crítico
        #{
            id => file_manager,
            start => {file_manager, start, []},
            restart => permanent,
            shutdown => 5000,
            type => worker
        }
        
        % Nota: tcp_server y discovery se inician desde p2p_node
        % porque necesitan parámetros dinámicos (puerto, node_id)
    ],
    
    {ok, {SupFlags, ChildSpecs}}.
