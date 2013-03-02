-module(cpcg_event_feed).
%-version('1.1')
-behaviour(gen_server).

-include("../include/backend.hrl").

%%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Interface
-export([
         start/2,
         stop/0,
         reload/1
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

-define(FEED_TIMEOUT, 1000).

% EVENT_HIST
% it is an histogram of the power function
-define(EVENT_HIST,
        [
         {1, "A"},
         {2, "B"},
         {3, "C"},
         {4, "D"}
        ]).

%%% RECORDS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(state,
        {timer_ref,
         turn
        }
       ).

%%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start(normal, _Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_args, []).

stop() -> gen_server:call(?MODULE, terminate).

reload(X) ->
    gen_server:cast(?MODULE, {reload, X}).

%%% GEN_SERVER CALLBACKS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(no_args) ->
    {ok, TRef} = timer:send_interval(?FEED_TIMEOUT, timeout),
    {ok, #state{timer_ref = TRef, turn = 5}}.

handle_call(terminate, _, State) ->
    {stop, normal, State}.

handle_cast({send_event}, State) ->
    % get a random item in the event distribution sample
    Event = lists:nth(random:uniform(length(?EVENT_HIST)), ?EVENT_HIST),

    % loop and feed the events via API
    io:format("send event to application ~p~n", [Event]),
    cpcg_worker:post_event(Event),

    {noreply, State};

handle_cast({reload, X}, S) ->

    io:format("turn : ~p~n", [S#state.turn]),

    NewS = case S#state.turn of
               -1 ->
                   {ok, TRef} = timer:send_interval(?FEED_TIMEOUT, timeout),
                   #state{timer_ref = TRef, turn = X};
               _ -> S
           end,
    {noreply, NewS}.


handle_info(timeout, State) ->
    % come back from timeout, send_event
    gen_server:cast(?MODULE, {send_event}),

    io:format("State: ~p~n", [State#state.turn]),

    case State#state.turn == 0 of
        true -> {ok, cancel} = timer:cancel(State#state.timer_ref);
        false -> ok
    end,

    {noreply, State#state{turn = State#state.turn-1}}.

code_change(_, State, _) ->
    {ok, State}.
terminate(_, _) ->
    ok.

%%% INTERNAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%