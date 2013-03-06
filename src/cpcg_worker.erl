%% @doc worker module, offers an API for posting event in its queue, in an 
%% asynchronous manner.

-module(cpcg_worker).
-behaviour(gen_server).

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
-define(PROCESSING_TIME, 50).
-define(BATCH_LENGTH_LIMIT, 500).

-type event() :: record().

%%% RECORDS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(state,
        {
          batch = [],
          batch_length = 0
        }
       ).


%%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start(Instance :: integer()) -> ok.
%% @doc start the worker process
%% @todo Finish writing the documentation.
start(Instance) ->
    Name = atom_to_list(?MODULE) ++ "_" ++ integer_to_list(Instance),
    gen_server:start_link({local, list_to_atom(Name)}, ?MODULE, no_args, []).

-spec stop(Name) -> ok when
      Name :: pid() | atom().
%% @doc stop the worker process
stop(Name) -> gen_server:call(Name, stop).

-spec post_event(Name, Event) -> event_batched|batch_full when 
      Name :: pid() | atom(),
      Event :: event().
%% @doc post an event in the process queue. 
post_event(Name, Event) ->
    gen_server:call(Name, {new_event, Event}).

%%% GEN_SERVER CALLBACKS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @private
init(no_args) ->
    State = #state{},
    {ok, State}.

%% @private
handle_call({new_event, Event}, _From, #state{batch = L,
                                              batch_length = QL})
  when QL < ?BATCH_LENGTH_LIMIT-1 ->
    %% put event in the batch
    NewS = #state{batch_length = QL + 1,
                  batch = lists:append(L, [Event])},
    {reply, event_batched, NewS};

handle_call({new_event, Event}, _From, #state{batch = L,
                                              batch_length = QL})
  when QL == ?BATCH_LENGTH_LIMIT-1 ->
    %% put last event in the batch
    NewS = #state{batch_length = QL + 1,
                  batch = lists:append(L, [Event])},

    self() ! {begin_process},
    %% send a process request
    {reply, batch_full, NewS};

%% this can not happen, process_event is locking
%% handle_call({new_event, _Event}, _From, S = #state{batch_length = QL})
%%   when QL > ?BATCH_LENGTH_LIMIT-1 ->
%%     {reply, overload, S};

handle_call({stop}, _From, State) ->
    {stop, normal, State}.

%% @private
handle_cast(_, S) ->
    {noreply, S}.

%% @private
handle_info({begin_process}, S) ->
    process_event(S),
    {noreply, #state{}}. %% default state

%% @private
code_change(_, State, _) ->
    {ok, State}.
%% @private
terminate(_, _) ->
    ok.

%%% INTERNAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
process_event(#state{batch = B, batch_length = BL}) ->
    %% timer sleep 2 ms
    %% will limit the msg process at < 500/sec
    io:format("worker ~p - process ~p events~n",
              [self(), BL]),

    lists:map(fun(_) ->
                      timer:sleep(2)
              end,
              B).
