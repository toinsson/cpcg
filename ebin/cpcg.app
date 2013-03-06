{application, cpcg,
 [
  {vsn, "1.0.0"},
  {modules,
   [
    cpcg,
    cpcg_worker,
    cpcg_worker_sup,
    cpcg_event_feed,
    cpcg_test,
    cpcg_batch_server
   ]
  },
  {applications, [stdlib, kernel]},
  {registered, [cpcg, cpcg_event_feed, cpcg_worker, cpcg_worker_sup]},
  {mod, {cpcg_batch_server, []}}
 ]}.
