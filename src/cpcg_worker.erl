-module(cpcg_worker).
%-version('1.1')
-behaviour(gen_server).

%%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Interface
-export([
         start/2,
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
-define(PROCESSING_TIME, 1000).

%%% RECORDS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(state, 
        {
          queue = [],
          queue_length = 0
        }
        ).


%%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start(normal, _Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_args, []).

stop() -> gen_server:call(?MODULE, stop).

post_event(Event) ->
    io:format("worker, post_event~n"),    
    io:format("~s~n", [?MODULE]),    

    gen_server:cast(?MODULE, {new_event, Event}).
    

%%% GEN_SERVER CALLBACKS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(no_args) ->
    State = #state{},
    {ok, State}.

handle_call(Call, _, State) ->
    {stop, {"Can not handle cast", Call}, State}.

handle_cast({new_event, Event}, S) ->
    % get new_event and put on processing queue
    NewS = S#state{queue = lists:append(S#state.queue, [Event]),
                   queue_length = S#state.queue_length + 1},
    
    % trigger event processing
    io:format("worker, got event~n"),    
    %io:format("~p~n", [NewS]),
    
    %gen_server:cast(?MODULE, {process}),
    {noreply, NewS};

handle_cast({process}, S) ->
    % if queue not empty, process
    NewS = case S#state.queue == [] of
               true ->
                   [H|T] = S#state.queue,
                   process_event(H),
                   gen_server:cast(?MODULE, {process}),
                   S#state{queue = T, 
                           queue_length = S#state.queue_length - 1};
               false ->
                   S
            end,
    % else
    {noreply, NewS}.

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
