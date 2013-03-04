cpcg
====

The purpose of the application is to handle a feed of event, and dump to a
database as batches.
The bottleneck would the process receiving the event feed.
To overcome this, the receiving process - name cpcg_batch_server - is spawning
an arbitrary number of worker - OTP gen_ser, module cpcp_worker - under the
supervisation of a supervisor cpcg_worker_sup.

The feed use an asynchronous call the batch server. It will redirect the event
as a synchrinous call towards the current worker process, which will save the
event in a list. When the list limit is reached, the batch processing is
launched and an other worker receive the feed.

USAGE:
> erl
(erl) > make:all([load]).
(erl) > application:start(cpcg).
(erl) > cpcg_event_feed:start().
(erl) > cpcg_event_feed:reload(100).
(erl) > application:stop(cpcg).
(erl) > cpcg_event_feed:stop().

TODO:
- create the worker when needed.
- transform event_feed as an EUNIT test
- adapt EVENT_HIST to realistic distribution
- use REBAR
- remove the dead code
- add supervision for batch_server and supervisor
