
The Hacker Games protocol v0.1
==============================

The Hacker Games protocol is a protocol regarding the
communication between the engine program, responsible for mediating a
game between one or more players, who play the game, via the engine
program. The players are never in direct communication.

Low level details
-----------------

Communication takes places over TCP network sockets, so standard
network latency should be expected.

Connection initialization
-------------------------

The engine program must bind to the socket given to it as its first
argument, and then accept as many players as is appropriate to the
gametype. The number of players will be defined in the engine's
configuration file, and the Hacker Games platform (or other engine
runner) will be responsible for insuring that only the defined number
of players attempt to connect to the engine's port.

Each connection formed here represents one player, and the engine can
now assign each an identification number, which will be used for
identification purpouses. In haskell, a structure representing a
player might look like this:

    data Player = Player { playerSocket :: Handle
                         , playerID :: Integer}

Once connections have been established, the engine should send the
version of the HGP it will conform to. No reply is expected from the
reciever, as they can simply close the socket if they do not conform
to the given version of the HGP which will indicate a techical fault,
and cancel the game.

Messages
--------

A message in The Hacker Games protocol (HGP from this point forwards)
is a high level one direction communication. It can be sent either
from the engine to the player, or vice versa. For the sake of
convinience, we will call the sender "sender" and the receiver
"receiver".

<pre>

+--------+               +----------+
| sender | ------------> | reciever |
+--------+  TGP message  +----------+

</pre>

A TGP message consists of two TCP read/write pairs. The sender will
generate a message id, which should be a random number. The message
from the sender should include the message id. The reciever should
aknowledge the message has been recieved by returning the message
id. The sender should then check if the message id returned by the
reciever is the same as the message id it sent. If it is, then the
message has succeeded. If it has not, then the message has failed.

<pre>

+--------+               +----------+
| sender | ------------> | reciever |
+--------+  msg + msgID  +----------+

+--------+               +----------+
| sender | <------------ | reciever |
+--------+      msgID    +----------+

</pre>

Message Failure
---------------

When a message fails (that is, the message id returned by the reciever
is not equal to that sent by the sender), the reciever is assumed to
be at fault. If the sender is the engine, then the implications of
this are clear: the player that is the reciever has failed to conform
to the spec, and looses the game by default. On the other hand, if the
sender is the player, a failed message indicates a problem with the
engine (or with the connection). The ideal solution in this case would
be to resend the message, but TGP does not currently support that, so
if this happens, the reciever should request game cancellation, and
file a bug report.

Message layout
--------------

The first message from sender to reciever should start with the line
`START {message id}` where message id is the message's unique id. This
should be followed by the message body, a strictly encoded JSON
message (that is, with only an object or array at the top level). The
message should end with a line along the lines of `END {message id}`.

Here is an example first message sending the data `{"foo": "bar"}`:

    START 203912
    {"foo": "bar"}
    END 203912

The reciever should reply with a single line `OK {message id}`. So in
the example above, the reciever would reply with:

    OK 203912
