-module(cpcg_batch_server).
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
-define(NUMBER_OF_WORKER, 10).

%%% RECORDS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(state,
        {
          cur_worker = [],
          worker_list = []
        }).

%%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start(normal, _Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_args, []).

stop() -> gen_server:call(?MODULE, stop).

post_event(Event) ->
    %% io:format("batch server, post_event~n"),
    gen_server:cast(?MODULE, {new_event, Event}).

%%% GEN_SERVER CALLBACKS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(no_args) ->
    %% start the worker supervisor
    cpcg_worker_sup:start_link(),

    %% instantiate the children
    WorkerPidList =
        lists:map(
          fun(X) ->
                  {ok, Pid} = supervisor:start_child(cpcg_worker_sup,
                                                     [X]),
                  Pid
          end,
          lists:seq(1, ?NUMBER_OF_WORKER)
         ),

    [H|T] = WorkerPidList,

    S = #state{cur_worker = H,
               worker_list = T},

    {ok, S}.

handle_call(stop, _, State) ->
    %% stop the supervisor
    cpcg_worker_sup:stop(),
    {stop, normal, State}.

handle_cast({new_event, Event}, S = #state{cur_worker = Pid}) ->
    %% get the current worker
    Pid = S#state.cur_worker,
    Reply = cpcg_worker:post_event(Pid, Event),

    NewS = case Reply of
               event_batched -> S;

               batch_full -> %% change worker
                   [NewWorker|Tail] = S#state.worker_list,
                   #state{cur_worker = NewWorker, worker_list = Tail ++ [Pid]}
           end,

    {noreply, NewS}.

handle_info(timeout, State) ->

    {noreply, State}.

code_change(_, State, _) ->
    {ok, State}.
terminate(_, _) ->
    ok.

%%% INTERNAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
