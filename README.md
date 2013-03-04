cpcg
====

The purpose of the application is to handle a feed of event, and dump to a
database as batches.
The bottleneck would the process receiving the event feed.
To overcome this, the receiving process - name cpcg_batch_server - is spawning
an arbitrary number of worker - OTP gen_ser, module cpcp_worker - under the
supervisation of a supervisor cpcg_worker_sup.

The feed use an asynchronous call the batch server. It will redirect the event
as a synchronous call towards the current worker process, which will save the
event in a list. When the list limit is reached, the batch processing is
launched and an other worker receive the feed.

The dimension of the system is the following:
event_feed takes a uniformly distributed number of events to send per second
within the list [1000, 1000, 1000, 17000]. This will give an average of 5000
messages per second, with a burst in 25% of the time.
The workers are 10 distributed processes, that can "batch" 500 events per
second. This gives the capcaity of 5000 needed. (Some margins can be added by
increasing the number of workers).

USAGE:
application:start(cpcg).

cpcg_event_feed:start().

cpcg_event_feed:reload(X).

TODO:
- create the worker when needed.
- transform event_feed as an EUNIT test
- adapt EVENT_HIST to realistic distribution
- use REBAR
- remove the dead code
- add supervision for batch_server and supervisor
