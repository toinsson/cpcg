-module(cpcg_worker).
%-version('1.1')
-behaviour(gen_server).

%%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Interface
-export([
         start/1,
         post_event/2
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
-define(BATCH_LENGTH_LIMIT, 5000).

%%% RECORDS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(state,
        {
          ets_batch = [],%ets:new(?MODULE, []),
          queue_length = 0
        }
       ).


%%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, no_args, []).

stop(Name) -> gen_server:call(Name, stop).

post_event(Name, Event) ->
    io:format("worker, post_event~n"),
    io:format("~s~n", [Name]),

    gen_server:call(Name, {new_event, Event}).


%%% GEN_SERVER CALLBACKS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(no_args) ->
    State = #state{},

    {ok, State}.

handle_call({new_event, Event}, From, State) ->
    {reply, "Done writing", State}.

handle_cast({new_event, Event}, S) ->
    % get new_event and put on processing queue
    %ets:insert(S#state.ets_batch, {Event}),
    L = lists:append(S#state.ets_batch, [Event]),
    NewS = S#state{ets_batch = L,
                   queue_length = S#state.queue_length + 1},

    % case queue_length == BATCH_LENGTH_LIMIT
    % send worker full
    % trigger batch

    {noreply, NewS};

handle_cast({process}, S) ->
    {noreply, S}.

handle_info(timeout, State) ->
    % come back from timeout, send_event
    gen_server:cast(?MODULE, {send_event}),

    {noreply, State}.

code_change(_, State, _) ->
    {ok, State}.
terminate(_, _) ->
    ok.

%%% INTERNAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
process_event(Event) ->
    % print Event
    io:format("~p~n", [Event]),

    % timer sleep 20 ms
    % will limit the msg process at ~ 50/sec
    timer:sleep(20).
