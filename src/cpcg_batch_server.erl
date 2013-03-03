-module(cpcg_batch_server).
%-version('1.1')
-behaviour(gen_server).

% include the API

%%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Interface
-export([
         start/2,
         stop/0,
         post_event/1
        ]).

% Gen Server part
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

%%% DEFINES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(SPEC(MFA),
        {
          worker_sup,
          {cpcg_worker_sup, start_link, [MFA]},
          temporary,
          10000,
          supervisor,
          [ppool_worker_sup]
        }).

-define(NUMBER_OF_WORKER, 10).

%%% RECORDS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(state, 
        {
          cur_worker = [],
          queue = queue:new()
        }).

%%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start(normal, _Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_args, []).

stop() -> gen_server:call(?MODULE, terminate).

post_event(Event) ->
    io:format("batch server, post_event~n"),
    gen_server:cast(?MODULE, {new_event, Event}).

%%% GEN_SERVER CALLBACKS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(no_args) ->
    %S = #state{queue = Q},
    
    cpcg_worker_sup:start_link(),

    {ok, Pid} = supervisor:start_child(cpcg_worker_sup, [robin]),

    %% lists:map(fun(X) ->
    %%                   X,
    %%                   cpcg_worker_sup:start_child()
    %%           end,
    %%           lists:seq(1,?NUMBER_OF_WORKER)
    %%          ),
    
    S = #state{cur_worker = {Pid, robin}},
            %queue = queue:in({Pid, robin}, Q)},

    {ok, S}.

handle_call(terminate, _, State) ->
    {stop, normal, State}.

handle_cast({new_event, Event}, S) ->
    
    % get the current worker
    %% case queue:out(S#state.queue) of 
    %%     {{value, {Pid, Name}, Q}} ->
            
    {_Pid, Name} = S#state.cur_worker,

    Reply = cpcg_worker:post_event(Name, Event),
    io:format("Reply : ~s~n", [Reply]),
    %
     
    {noreply, S}.

handle_info(timeout, State) ->

    {noreply, State}.

code_change(_, State, _) ->
    {ok, State}.
terminate(_, _) ->
    ok.

%%% INTERNAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
