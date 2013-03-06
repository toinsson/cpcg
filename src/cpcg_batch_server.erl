-module(cpcg_batch_server).
-behaviour(gen_server).

-include("cpcg_def.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Interface
-export([start/0,
         start/1,
         start/2,
         stop/0,
         post_event/1
        ]).

%% Gen Server part
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

%%% DEFINES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%% RECORDS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(state,
        {
          cur_worker = [],
          worker_list = []
        }).

%%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc start the batch server
start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_args, []).
%% @doc start the batch server with configuration
start(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).
%% @doc start the batch server for application
start(normal, _Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_args, []).

%% @doc stop the batch server
stop() -> gen_server:call(?MODULE, stop).


%% @doc post an event
-spec post_event(Event :: cpcg_event()) -> ok.
post_event(Event) ->
    gen_server:cast(?MODULE, {new_event, Event}).

%%% GEN_SERVER CALLBACKS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @private
init(no_args) ->
    init_common(?DEFAULT_NUMBER_OF_WORKERS);
init(NumberOfWorkers) when is_integer(NumberOfWorkers) ->
    init_common(NumberOfWorkers).

%% @private
handle_call(stop, _, State) ->
    %% stop the supervisor
    cpcg_worker_sup:stop(),
    {stop, normal, ok, State}.

%% @private
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

%% @private
handle_info(timeout, State) ->
    {noreply, State}.
%% @private
code_change(_, State, _) ->
    {ok, State}.
%% @private
terminate(_, _) ->
    ok.

%%% INTERNAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init_common(NumberOfWorkers) ->
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
          lists:seq(1, NumberOfWorkers)
         ),

    [H|T] = WorkerPidList,

    S = #state{cur_worker = H,
               worker_list = T},

    {ok, S}.
