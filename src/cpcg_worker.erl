%% @doc worker module, offers an API for posting event in its queue, in an 
%% asynchronous manner.

-module(cpcg_worker).
-behaviour(gen_server).

-include("cpcg_def.hrl").

%%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Interface
-export([
         start/1,
         post_event/2,
         stop/1
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
          batch = [],
          batch_length = 0,
          callback = undef
        }
       ).


%%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc start the worker process
-spec start(Instance :: integer() | worker_cfg()) -> ok.
start(Cfg = #worker_cfg{id = Id}) ->
    Name = atom_to_list(?MODULE) ++ "_" ++ integer_to_list(Id),
    gen_server:start_link({local, list_to_atom(Name)}, ?MODULE, Cfg, []);
start(Instance) ->
    Name = atom_to_list(?MODULE) ++ "_" ++ integer_to_list(Instance),
    gen_server:start_link({local, list_to_atom(Name)}, ?MODULE, no_args, []).

%% @doc stop the worker process
-spec stop(Name) -> ok when Name :: pid() | atom().
stop(Name) -> gen_server:call(Name, {stop}).

%% @doc post an event in the process queue. Will reply with batch_full in case
%% the BATCH_LENGTH_LIMIT for the worker has been reached. 
-spec post_event(Name, Event) -> event_batched|batch_full when 
      Name :: pid() | atom(),
      Event :: string() | atom().
post_event(Name, Event) ->
    gen_server:call(Name, {new_event, Event}).

%%% GEN_SERVER CALLBACKS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @private
init(#worker_cfg{db_callback = Cb}) ->
    State = #state{callback = Cb},
    {ok, State};
init(no_args) ->
    Cb = fun(X) -> process_batch(X) end,
    State = #state{callback = Cb},
    {ok, State}.

%% @private
handle_call({new_event, Event}, _From, S = #state{batch = L,
                                                  batch_length = QL})
  when QL < ?BATCH_LENGTH_LIMIT-1 ->
    %% put event in the batch
    NewS = S#state{batch_length = QL + 1,
                   batch = lists:append(L, [Event])},
    {reply, event_batched, NewS};

handle_call({new_event, Event}, _From, S = #state{batch = L,
                                                  batch_length = QL})
  when QL == ?BATCH_LENGTH_LIMIT-1 ->
    %% put last event in the batch
    NewS = S#state{batch_length = QL + 1,
                   batch = lists:append(L, [Event])},
    self() ! {begin_process},
    {reply, batch_full, NewS};

%% this can not happen, process_batch is locking
%% handle_call({new_event, _Event}, _From, S = #state{batch_length = QL})
%%   when QL > ?BATCH_LENGTH_LIMIT-1 ->
%%     {reply, overload, S};

handle_call({stop}, _From, State) ->
    {stop, normal, ok, State}.

%% @private
handle_cast(_, S) ->
    {noreply, S}.

%% @private
handle_info({begin_process}, S) ->
    (S#state.callback)(S#state.batch),
    NewS = S#state{batch = [], batch_length = 0}, %% erase batch
    {noreply, NewS}. 

%% @private
code_change(_, State, _) ->
    {ok, State}.
%% @private
terminate(_, _) ->
    ok.

%%% INTERNAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
process_batch(B) ->
    BL = length(B),

    %% timer sleep 2 ms
    %% will limit the msg process at < 500/sec
    %% io:format("worker ~p - process ~p events~n",
    %%           [self(), BL]),
    io:format("worker ~p - process ~p events~n",
              [self(), BL]),

    lists:map(fun(_) ->
                      timer:sleep(2)
              end,
              B).
