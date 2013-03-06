cpcg
====

The purpose of the application is to handle a feed of event, and dump it to a
database as batches.
The event feed has a highly fluctuating rate of events per seconds, and might 
disturb the database writing.

To overcome this, the receiving process, cpcg_batch_server, is spawning an
arbitrary number of worker, cpcg_worker, under the supervisation of a
supervisor, cpcg_worker_sup in order to buffer chunks of the feed.

The feed use an asynchronous call per event to the batch server. It will redirect
the event as a synchronous call towards the current worker process, which will
save the event in a list. When the list limit is reached, the batch processing is
launched and an other worker receive the feed.

The dimension of the system is the following:
event_feed takes a uniformly distributed number of events to send per second
within the list [1000, 1000, 1000, 17000]. This will give an average of 5000
messages per second, with a burst in 25% of the time.
The workers are 12 distributed processes, that can "batch" 500 events per
second. This gives a bit over the capacity of 5000 needed.

To use/test the application:
- application:start(cpcg) 
- cpcg_io_stub:start() will send event for 10 secondes
- cpcg_io_stub:reload() will resend event for X secondes

TODO:
- add eunit test for load stability, e.g. long running test 
- adapt the batch_server and worker_sup to use the record configuration  

IMPROVMENTS:
- documentation
- create the worker when needed.
- use REBAR
- add supervision for batch_server and supervisor
