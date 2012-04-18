
The Hacker Games protocol v0.2
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

The HGP does not define much information on how communcation will take
place. All messages should be JSON encoded, and have an object as
their top level. Each message should have one newline in it; new lines
in strings should be encoded as \n.
