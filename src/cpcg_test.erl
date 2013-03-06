%% @doc EUNIT test module
-module(cpcg_test).

-include("cpcg_def.hrl").
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

writing_test_() ->
    {"send feed data and compare dump",
     {
       setup,
       fun start_worker/0,
       fun stop_worker/1,
       fun write/1
     }
    }.


%%% SETUP FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start() ->
    {ok, Pid1} = cpcg_io_stub:start(),
    {ok, Pid2} = cpcg_batch_server:start(?TEST_NUMBER_OF_WORKER),
    {Pid1, Pid2}.
stop(_) ->
    ok = cpcg_io_stub:stop(),
    ok = cpcg_batch_server:stop().

start_worker() ->
    cpcg_io_stub:start(),
    DbCallback = fun(X) -> cpcg_io_stub:put_batch(X) end,
    Cfg = #worker_cfg{id = 1, db_callback = DbCallback},
    {ok, Pid} = cpcg_worker:start(Cfg),
    Pid.

stop_worker(Pid) ->
    ok = cpcg_io_stub:stop(),
    ok = cpcg_worker:stop(Pid).

%%% ACTUAL TESTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

up_and_running({Pid1, Pid2}) ->

    [
     ?_assert(erlang:is_process_alive(Pid1)),
     ?_assertEqual(Pid1, whereis(cpcg_io_stub)),

     ?_assert(erlang:is_process_alive(Pid2)),
     ?_assertEqual(Pid2, whereis(cpcg_batch_server)),

     ?_assert(whereis(cpcg_worker_sup) /= undefined),

     lists:map(fun(X) ->
                       Name = atom_to_list(cpcg_worker_) ++ integer_to_list(X),
                       ?_assert(whereis(list_to_atom(Name)) /= undefined)
               end,
               lists:seq(1, ?TEST_NUMBER_OF_WORKER))
    ].

write(Pid) ->
    Feed = lists:seq(1, ?BATCH_LENGTH_LIMIT),
    lists:map(fun(Event) ->
                      cpcg_worker:post_event(Pid, Event)
              end,
              Feed),
    timer:sleep(1000),

    {batch, Batch} = cpcg_io_stub:get_batch(),
    [?_assert(Feed == Batch)].

%% add test that monitor the message queue in batch_server
%% by checking the state content through sys:get_status(cpcg_batch_server).
%% {status,<0.173.0>,
%%         {module,gen_server},
%%         [[{'$ancestors',[<0.172.0>]},
%%           {'$initial_call',{cpcg_batch_server,init,1}}],
%%          running,<0.172.0>,[],
%%          [{header,"Status for generic server cpcg_batch_server"},
%%           {data,[{"Status",running},
%%                  {"Parent",<0.172.0>},
%%                  {"Logged events",[]}]},
%%           {data,[{"State",
%%                   {state,<0.175.0>,
%%                          [<0.176.0>,<0.177.0>,<0.178.0>,<0.179.0>,<0.180.0>,
%%                           <0.181.0>,<0.182.0>,<0.183.0>,<0.184.0>|...]}}]}]]}
