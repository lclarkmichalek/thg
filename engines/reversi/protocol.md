The Hacker Games Reversi
========================

This document aims to set out the assumptions that THG reversi engine
makes, and what a player program needs to do to be successfull. All
messages will be JSON objects.

Initial Gamestate sync
----------------------

After standard HGP connection initialization, the first message that a
player will receive will contain the initial state of the game board,
and the player who will start the game. An example initial message
might look like this:

    {
    'board': [[-1, -1],
              [-1, -1]],
    'next_turn': 0,
    'you': 1,
    }

The board in this case is 2x2 (in reality it would be 8 by 8). In the
board, -1 represents no stone, and any other number represents a stone
placed by the player with that player id. The `'next_turn'` field of the
object is the player id of the player who will take the next
turn, and the `'you'` field is the id of the player to whom the
message was sent.

Standard Turn
-------------

A standard turn will begin with the player who is due to take a turn
sending a message of the following format to the engine:

    {
    'placed': [0, 0],
    }

Where the value of the `'placed'` field are the coordinates of the
stone that the player has elected to placed. The engine will then send
another message to both players:

    {
    'changed': [[0, 0], [1, 1], [2, 2]],
    'by': 1,
    'next_turn': 0,
    'game_status': 0
    }

The '`changed'` field is a list of all the stones that were placed
or changed in the last turn by the player with the player id
`'by'`. The `'next_turn'` field has the same purpous as in the
Initial Gamestate sync stage, to inform the player who will be taking
the next turn.

The `'game_status'` field is more complicated. It is a numerical value
that represents the status of the game. The values and their meanings
are enumerated below:

+---------+------------------------------------+
| Value   | Meaning                            |
+---------+------------------------------------+
| 0       | Game in progress                   |
| 1       | Game finished due to player win    |
| 2       | Game finished due to stalemate     |
| 3       | Game finished due to disconnect    |
| 4       | Game finished due to bad message   |
| 5       | Game finished due to bad move      |
+---------+------------------------------------+

While these don't inform the user who won the game, they can usually
infer the winner. However, there should be no need to, as THG platform
should receive the results of the game later on, and the player can
view them there.

If the game has finished, the player should close the socket, and the
engine will do the same.

Bad moves
---------

If the player returns with data that cannot be parsed or is an invalid
move, that player looses the game by default. The engine will not send
any specific error messages, just return a new message with the
relavant `'game_status'`.
