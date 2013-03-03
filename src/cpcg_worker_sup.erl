-module(cpcg_worker_sup).
-behaviour(supervisor).

%%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Interface

% supervisor part
-export([
         start_link/0, 
         init/1
        ]).

%%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
start_link() ->
    
    io:format("~s~n", [?MODULE]),
    
    supervisor:start_link({local, ?MODULE}, ?MODULE, no_args).
 
init(no_args) ->
    MaxRestart = 5,
    MaxTime = 3600,
    {ok, 
     {
       {simple_one_for_one, MaxRestart, MaxTime},
       [
        {cpcg_worker,
         {cpcg_worker, start, []},
         temporary, 
         5000, 
         worker, 
         [cpcg_worker]
        }
       ]
     }
    }.
