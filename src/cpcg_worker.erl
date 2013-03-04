-module(cpcg_worker).
%-version('1.1')
-behaviour(gen_server).

%%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Interface
-export([
         start/1,
         post_event/2,
         stop/1
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
-define(PROCESSING_TIME, 50).
-define(BATCH_LENGTH_LIMIT, 500).

%%% RECORDS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(state,
        {
          batch = [],%ets:new(?MODULE, []),
          batch_length = 0
        }
       ).


%%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start(Instance) ->
    Name = atom_to_list(?MODULE) ++ "_" ++ integer_to_list(Instance),
    gen_server:start_link({local, list_to_atom(Name)}, ?MODULE, no_args, []).

stop(Name) -> gen_server:call(Name, stop).

post_event(Name, Event) ->
    %% io:format("worker ~p, post_event~n", [Name]),

    gen_server:call(Name, {new_event, Event}).

%% process() ->
%%     gen_server:cast(Name, {process}).


%%% GEN_SERVER CALLBACKS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(no_args) ->
    State = #state{},

    {ok, State}.

handle_call({new_event, Event}, _From, #state{batch = L,
                                              batch_length = QL})
  when QL < ?BATCH_LENGTH_LIMIT-1 ->
    % put event in the batch
    NewS = #state{batch_length = QL + 1,
                  batch = lists:append(L, [Event])},
    {reply, event_batched, NewS};

handle_call({new_event, Event}, _From, #state{batch = L,
                                              batch_length = QL})
  when QL == ?BATCH_LENGTH_LIMIT-1 ->
    % put event in the batch
    NewS = #state{batch_length = QL + 1,
                  batch = lists:append(L, [Event])},

    self() ! {begin_process},
    % send a process request
    {reply, batch_full, NewS};

%% this can not happen, process_event is locking
handle_call({new_event, _Event}, _From, S = #state{batch_length = QL})
  when QL > ?BATCH_LENGTH_LIMIT-1 ->
    {reply, overload, S};

handle_call({stop}, _From, State) ->
    {stop, normal, State}.

handle_cast(_, S) ->
    {noreply, S}.

handle_info({begin_process}, S) ->
    process_event(S),

    NewS = #state{},
    {noreply, NewS}.

code_change(_, State, _) ->
    {ok, State}.
terminate(_, _) ->
    ok.

%%% INTERNAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
process_event(#state{batch = B, batch_length = BL}) ->
    % timer sleep 2 ms
    % will limit the msg process at < 500/sec
    io:format("worker ~p - process ~p events~n",
              [self(), BL]),

    lists:map(fun(_) ->
                      timer:sleep(2)
              end,
              B).
