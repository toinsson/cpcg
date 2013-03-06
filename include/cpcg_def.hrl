-define(DEFAULT_NUMBER_OF_WORKERS, 12).
-define(PROCESSING_TIME, 50).
-define(BATCH_LENGTH_LIMIT, 500).

-record(cpcg_event, {time, value}).
-type cpcg_event() :: #cpcg_event{}.

-record(worker_cfg, {id, db_callback}).
-type worker_cfg() :: #worker_cfg{}.

-record(batch_server_cfg,
        {
          number_of_workers = ?DEFAULT_NUMBER_OF_WORKERS,
          worker_cfg = #worker_cfg{},
          worker_sup_cfg
        }
       ).
