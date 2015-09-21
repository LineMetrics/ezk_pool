# ezk_pool

##Status : deprecated
  Why ? Would need a special pool-implementation for working efficiently with zookeeper-watches.
  Do not use for production


ezk + worker_pool

Pooled Zookeeper-Connections for Erlang

# Changes
added handling of  zk-watches:

a big number of zookeeper watches held by a number of worker-processes

switched from poolboy to inaka/worker_pool
