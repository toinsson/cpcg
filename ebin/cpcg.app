{application, cpcg,
 [
  {vsn, "1.0.0"},
  
  {modules, 
   [
    cpcg, 
    cpcg_worker, 
    cpcg_pool_sup,
    cpcg_event_feed
   ]
  },
  
  {applications, [stdlib, kernel]},
  
  {registered, [cpcg, cpcg_event_feed, cpcg_worker]},
  
  {mod, {cpcg_worker, []}}

  %% {env, [
  %%   {answers, {<<"Yes">>, <<"No">>, <<"Doubtful">>,
  %%              <<"I don't like your tone">>, <<"Of course">>,
  %%              <<"Of course not">>, <<"*backs away slowly and runs away*">>}}
  %% ]}
 ]}.
