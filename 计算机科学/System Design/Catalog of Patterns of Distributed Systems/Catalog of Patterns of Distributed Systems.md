

- https://martinfowler.com/articles/patterns-of-distributed-systems/


Clock-Bound Wait  
Wait to cover the uncertainty in time across cluster nodes before reading and writing values so that values can be correctly ordered across cluster nodes.

Consistent Core  
Maintain a smaller cluster providing stronger consistency to allow the large data cluster to coordinate server activities without implementing quorum-based algorithms.

Emergent Leader  
Order cluster nodes based on their age within the cluster to allow nodes to select a leader without running an explicit election.

Fixed Partitions  
Keep the number of partitions fixed to keep the mapping of data to partition unchanged when the size of a cluster changes.

Follower Reads  
Serve read requests from followers to achieve better throughput and lower latency

Generation Clock  
A monotonically increasing number indicating the generation of the server.

Gossip Dissemination  
Use a random selection of nodes to pass on information to ensure it reaches all the nodes in the cluster without flooding the network

HeartBeat  
Show a server is available by periodically sending a message to all the other servers.

High-Water Mark  
An index in the write-ahead log showing the last successful replication.

Hybrid Clock  
Use a combination of system timestamp and logical timestamp to have versions as date and time, which can be ordered

Idempotent Receiver  
Identify requests from clients uniquely so you can ignore duplicate requests when client retries

Key-Range Partitions  
Partition data in sorted key ranges to efficiently handle range queries.

Lamport Clock  
Use logical timestamps as a version for a value to allow ordering of values across servers

Leader and Followers  
Have a single server to coordinate replication across a set of servers.

Lease  
Use time-bound leases for cluster nodes to coordinate their activities.

Low-Water Mark  
An index in the write-ahead log showing which portion of the log can be discarded.

Majority Quorum  
Avoid two groups of servers making independent decisions by requiring majority for taking every decision.

Paxos  
Use two consensus building phases to reach safe consensus even when nodes disconnect

Replicated Log  
Keep the state of multiple nodes synchronized by using a write-ahead log that is replicated to all the cluster nodes.

Request Batch  
Combine multiple requests to optimally utilise the network

Request Pipeline  
Improve latency by sending multiple requests on the connection without waiting for the response of the previous requests.

Request Waiting List  
Track client requests which require responses after the criteria to respond is met based on responses from other cluster nodes.

Segmented Log  
Split log into multiple smaller files instead of a single large file for easier operations.

Single-Socket Channel  
Maintain the order of the requests sent to a server by using a single TCP connection

Singular Update Queue  
Use a single thread to process requests asynchronously to maintain order without blocking the caller.

State Watch  
Notify clients when specific values change on the server

Two-Phase Commit  
Update resources on multiple nodes in one atomic operation

Version Vector  
Maintain a list of counters, one per cluster node, to detect concurrent updates

Versioned Value  
Store every update to a value with a new version, to allow reading historical values.

Write-Ahead Log  
Provide durability guarantee without the storage data structures to be flushed to disk, by persisting every state change as a command to the append only log.