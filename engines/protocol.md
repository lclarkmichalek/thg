
The Hacker Games protocol v0.2.1
================================

The Hacker Games protocol is a protocol regarding the
communication between the engine program, responsible for mediating a
game between one or more players, who play the game, via the engine
program. The players are never in direct communication.

Low level details
-----------------

Communication takes places over TCP network sockets, so standard
network latency should be expected.

Connection initialisation
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

Once connections have been established, the engine should send a
message containing the version of HGP they conform to. No reply is
expected from the reciever, as they can simply close the socket if
they do not conform to the given version of the HGP which will
indicate a techical fault, and cancel the game. The message should
look like this:

    {"version": "0.2.1"}

Messages
--------

The HGP does not define much information on how communcation will take
place. All messages should be JSON encoded, and have an object as
their top level. Each message should have one newline in it; new lines
in strings should be encoded as \n.

TwoPlayerBoard
--------------

The HG-core library includes the HG.Engines.TwoPlayerBoard module,
which allows the easy creation of many two player board
games. Messages here come in standard formats:

Full game sync
--------------

The full game sync comes at the start of the game. It is a JSON
encoded message, and should look like this:

    {
        'board': [[-1, -1, -1], [-1, -1, -1], [-1, -1, -1]],
        'next_turn': 2,
        'you': 1
    }

This full sync message might be for a game of tick tack toe for
example. The board will be a matrix where -1 represents no token
placed, and any numbers will represent a token of that player's player
id. The `next_turn` value is the player id of the player who has the
next turn. The `you` value is your player id.

Game update
-----------

After every move, you will receive an update message in this form:

    {
       'changed': [[1, 1], [0, 1]],
       'by': 3,
       'next_turn': 2,
       'game_status': 1
    }

The `changed_value` is a list of all the coordinates of the tokens
that have changed in the past turn. They will have changed to the
player id in the `by` field. `next_turn` is the player id of the
player who has the next turn. `game_status` is the status of the game,
which can be one of: 

    Playing = 0
    PlayerWin = 1
    Stalemate = 2
    Disconnect = 3
    BadMessage = 4
    BadMove = 5

When a message without status 0 is sent, the game is over, and the
player can terminate.

Sending moves
-------------

The TwoPlayerBoard engine expects a message in the form:

    {
        'from': [0, 0],
        'to': [1, 1]
    }

The `from` attribute is optional, for games where tokens are moved,
not just placed.
