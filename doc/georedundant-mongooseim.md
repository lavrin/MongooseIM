# Active-active / georedundant MongooseIM

Radek Szymczyszyn _<radoslaw.szymczyszyn@erlang-solutions.com>_ <br />
Kraków, 2016-2017

## Background

[MongooseIM](https://github.com/esl/MongooseIM) is an [XMPP](http://xmpp.org/)
server written in Erlang.

XMPP is an XML based instant messaging protocol.
While it is possible to transfer XMPP traffic over many types of
connections (WebSockets, HTTP long polling), the basic mode of operation
is a long-lived TCP connection.

While most XMPP server implementations operate on a single host,
MongooseIM (_MIM_) presents a Distributed Erlang cluster of machines as an XMPP server.

Session information in a MIM cluster is distributed using an Mnesia RAM-only table.
Therefore, session data is transient.
It's cleaned up by remaining members if a single node crashes or is brought down.
It's lost completely if the whole cluster goes down.
Unless otherwise stated (i.e. cases of [BOSH][bosh] - XMPP over HTTP,
and [Stream Resumption][xep0198] extension),
session lifetime is tied to the lifetime of a TCP connection.

[bosh]: http://xmpp.org/extensions/xep-0206.html
[xep0198]: http://xmpp.org/extensions/xep-0198.html

XMPP defines a simple routing mechanism for forwarding messages addressed
to clients who do not belong to the XMPP domain of the server receiving messages,
or _stanzas_ as they're called in XMPP parlance.
This mechanism is called _server to server_ (s2s) communication or _XMPP federation_.
In other words,
**federation allows to reroute XMPP messages between different XMPP servers**.
However, the route length is limited to one hop.

Due to the constraints of Mnesia, a practical "safe" MongooseIM cluster size
is approximately 10 nodes (though clusters up to 20+ nodes in size appear
in production systems).
Although this limitation puts a limit to the size of the cluster,
it's only one of the factors which impact its total user / session /
connection capacity.
The most important factor is the amount of available memory on each
of the nodes making up the cluster.

XMPP defines the address format used to route stanzas in the network to
unambiguously identify entities: clients, servers and components.
An XMPP address, historically called a _JID_, contains a _domain_ part
which is looked up in the DNS system to obtain an address for the network
host the XMPP client should connect to in order to gain access to the
XMPP network.
Moreover, the XMPP domain is also used to identify and connect federated servers.
A JID can be informally described as a `<user>@<domain>/<resource>` pattern,
where only the `<domain>` part is required.
Some example JIDs with their conventional meaning are:

-   `example.com` - denotes an XMPP server

-   `muc.example.com` - denotes an XMPP service available at `example.com`

-   `example.com/watchdog` - denotes a service available at `example.com`,
    though this pattern is rarely used in the XMPP standards

-   `alice@example.com` - denotes a user of the network

-   `alice@example.com/phone` - denotes a particular device or client
    application of the particular user

XMPP and its extensions also define some persistent relationships between
entities on the network.
The most basic one is bidirectional presence subscription.
Since a JID is the unique identifier of an XMPP entity,
these relationships are modeled using JIDs.
This will prove important in later discussion.


## Use cases

1.  Alice, who lives in New York, goes on a trip to Tokyo.
    To avoid service quality degradation, her device should connect
    to a geographically colocated data centre.

2.  Bob is a policeman and uses his XMPP connected device to coordinate
    with other squads in the vicinity.
    In case of a power failure or a natural disaster bringing the main
    data centre down, this connectivity has to remain unaffected.

3.  Charlie is a couch potato but also a technology fan owning
    an XMPP-connected phone, tablet, phablet, laptop and fridge.
    All these devices use different links (GSM, fiber, municipal
    wireless network).
    The DNS server or service provider might load-balance them across
    different data centres, but Charlie still wants a consistent
    experience across all devices: synchronized conversations,
    access to history, a setting change on one device taking immediate
    effect on all the rest.


## State of the Art

Deployments of MongooseIM which have to span several geographical
locations or exceed the capacity of a single cluster rely on XMPP federation
to route messages between clusters or data centres.

Due to limitations of XMPP addressing, users of such a network have to be sharded
and are "persistently allocated" to a particular data centre.
A user identified as _alice@us-west.example.com_ can't
interchangeably connect to _us-west.example.com_ and _us-east.example.com_,
simply because for any XMPP server _alice@us-west.example.com_ and _alice@us-east.example.com_
are _distinct entities_ (due to reasons mentioned in the _Background_):

- they use different XMPP addresses / JIDs - the basic method of identification used in XMPP,

- their persistent data is tied to their JIDs.

Let's analyze the above in the light of the previously defined use cases.


## Use case analysis

1.  Alice doesn't travel between New York and Tokyo all that often.
    First login to a data centre (_DC_ from now on) in a new geographical
    location could trigger a migration of data from the old data centre
    to the new one. There are two options:

    -   Both DCs use the same XMPP domain, therefore DNS could redirect
        to different addresses depending on geolocation.
        Data has to be fetched from the remote/previous DC,
        but doesn't have to be modified in any way.
        At the same time, **this architectural decision makes communication
        between DCs (an active-active scenario) impossible as XMPP federation
        only works between different domains.**

    -   Alternatively, each DC could be identified by a location prefix:
        `ny.example.com` and `tokyo.example.com`.
        Alice's client application could contact an external service
        (possibly also DNS) which based on her location would redirect
        her to the correct DC.
        Since DCs are effectively different XMPP servers,
        it's perfectly posssible for users of both of them to communicate
        with one another - XMPP federation works.
        However, on Alice's first login to a new DC a data migration is triggered -
        at this point all database entries which referred to
        `alice@ny.example.com` have to be rewritten to `alice@tokyo.example.com`.
        Moreover, data of other users of the system (either in NY or Tokyo!)
        might also need to be rewritten if Alice's JID was mentioned in
        those other users' records.
        **The dependency chain of changes proves this approach
        to be complex and infeasible in practice.**

2.  Bob's situation is very similar to Alice's,
    with the following differences:

    -   A lazy data migration might not be possible - if the "previous" DC is defunct.

    -   Service degradation might be acceptable - simple messaging,
        the critical part of the system,
        could be provided if some user data (recipient addresses) is stored
        on the client device without migrating any persistent data.

3.  Charlie's case imposes the highest requirements on the XMPP system.
    It requires complete detachment of a user identity from a DC providing
    him the service _at a particular moment in time_.
    This requires that:

    -   Users from different DCs can freely communicate with one another.

    -   Users can freely change DCs they connect to between connections.

    -   No matter what DC a user connects to, his/her persistent data is fresh.
        This will require live/streaming replication of data between DCs, 
        which in turn renders any approach with "record rewriting" utterly inapplicable.

Let's sum up the requirements drawn above:

-   Due to data modelling constraints, XMPP entities have to be identified with JIDs.
    JIDs contain a domain part which is subject to lookups using the DNS service.
    XMPP servers either have to use distinct domains so that XMPP federation works between them,
    or use the same domain but _shard the universe_ and eliminate communication between the shards.

-   Georedundancy / high availability with limited functionality may be more important
    than a full set of features with spurious downtimes.

-   An XMPP user should be freely available to switch between DCs,
    but maintain full connectivity to all his/her contacts,
    even those in other DCs than his/her current one.


## Prototype solution

[A prototype MongooseIM extension][mim:aaproto] addresses the above requirements by:

-   Modelling an XMPP data centre / cluster as a separate entity from an XMPP server.
    This allows to provide multiple addresses for one XMPP server,
    for example: `ny.example.com`, `tokyo.example.com`.
    These addresses can be used to route traffic using regular XMPP federation.

-   A thin XMPP protocol extension, to wrap stanzas which have to be
    exchanged using the above method, but their original sender's and recipient's
    addresses are in the main server domain (e.g. `example.com`).
    This allows to pass stanzas intact,
    but still provide an addressing layer between data centres.

-   _A session replication mechanism_ (currently a Mnesia event stream over plain TCP)
    consisting of a sender and receiver processes and a _remote session table_.
    This session contains entries about users available in other data centres.

-   Modification of the MongooseIM routing algorithm to provide for
    federation based routing of stanzas to recipients in the _remote session table_.

[mim:aaproto]: https://github.com/erszcz/MongooseIM/tree/active-active-proto

The prototype provides:

-   A way to deploy a MongooseIM-based XMPP network in an active-active fashion.
    Multiple data centres can be online at the same time and XMPP traffic
    (basic messaging) between users connected to any of them is possible.

-   Session information exchange without the use of Distributed Erlang
    between data centres.
    Distributed Erlang is not suitable for distribution across long distances.
    This part of the protocol might require substituting by a message queue
    (RabbitMQ) or a database suitable for "long range" data replication.

The prototype doesn't provide:

-   Any kind of persistent data migration / replication across DCs.

-   _A reliable signalling mechanism_ to detect DC failures.
    This is required to promptly clean up the _remote session table_
    in case of a DC failure.

_The session replication mechanism_ and _the reliable signalling mechanism_
are conceptually tied together.
The latter could be based on etcd (a Raft implementation),
plumtree (a fast converging gossip protocol) or Riak Core.
This point requires further consideration.
