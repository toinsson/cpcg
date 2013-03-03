{application, cpcg,
 [
  {vsn, "1.0.0"},
  
  {modules, 
   [
    cpcg, 
    cpcg_worker, 
    cpcg_worker_sup,
    cpcg_event_feed,
    cpcg_batch_server
   ]
  },
  
  {applications, [stdlib, kernel]},
  
  {registered, [cpcg, cpcg_event_feed, cpcg_worker, cpcg_worker_sup]},
  
  {mod, {cpcg_batch_server, []}}

  %% {env, [
  %%   {answers, {<<"Yes">>, <<"No">>, <<"Doubtful">>,
  %%              <<"I don't like your tone">>, <<"Of course">>,
  %%              <<"Of course not">>, <<"*backs away slowly and runs away*">>}}
  %% ]}
 ]}.
