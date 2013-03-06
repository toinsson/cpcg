%% @doc EUNIT test module
-module(cpcg_test).
-include_lib("eunit/include/eunit.hrl").


-define(TEST_NUMBER_OF_WORKER, 5).

%%% TESTS DESCRIPTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
up_and_running_test_() ->
    {"The event_feed, batch_server and workers are started, "
     "have a registered name and can be stopped cleanly",
     {
       setup,
       fun start/0,
       fun stop/1,
       fun up_and_running/1}
    }.

%% writing_test_() ->
%%     {"send feed data and compare dump"
%%      {
%%        setup,
%%        fun start_with_config/0,
%%        fun stop/1,
%%        fun write/1}
%%     }.


%%% SETUP FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start() ->
    {ok, Pid1} = cpcg_event_feed:start(),
    {ok, Pid2} = cpcg_batch_server:start(?TEST_NUMBER_OF_WORKER),
    {Pid1, Pid2}.

%% start_with_config() ->
%%     Config = #config{number_of_worker = X,
%%                      dump_fun = Y},
%%     %% {ok, Pid1} = cpcg_event_feed:start(),
%%     {ok, Pid2} = cpcg_batch_server:start(Config),
%%     {Pid1, Pid2}.


stop(_) ->
    ok = cpcg_event_feed:stop(),
    ok = cpcg_batch_server:stop().

%%% ACTUAL TESTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

up_and_running({Pid1, Pid2}) ->
    [
     ?_assert(erlang:is_process_alive(Pid1)),
     ?_assertEqual(Pid1, whereis(cpcg_event_feed)),

     ?_assert(erlang:is_process_alive(Pid2)),
     ?_assertEqual(Pid2, whereis(cpcg_batch_server)),

     ?_assert(whereis(cpcg_worker_sup) /= undefined),

     lists:map(fun(X) ->
                       Name = atom_to_list(cpcg_worker_) ++ integer_to_list(X),
                       ?_assert(whereis(list_to_atom(Name)) /= undefined)
               end,
               lists:seq(1, ?TEST_NUMBER_OF_WORKER))
    ].

%%
%% write(_) ->
%%     Feed = [],
%%     lists:map(fun(Event) ->
%%                       cpcg_batch_server:post_event(Event)
%%               end,
%%               Feed)
