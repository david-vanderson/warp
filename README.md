warp
====

Coop Networked Game in Racket

Depends on mode-lambda: raco pkg install mode-lambda

Start a server: racket server.rkt

Start each client: racket client.rkt

The game is primarily mouse driven with keyboard shortcuts.  There are two extra keyboard shortcuts:
- tab toggles showing more information on screen
- backtick toggles showing the whole map

This is a coop game!  It is very difficult to play with just a single person.  You'll generally want at least one person to pilot a ship and another person to sit at weapons.  To make this a bit easier, the AI controls any station that a player is not occupying.


status
----

Still in development, but playable.

In the "Pilot Training" scenario, each player gets their own fighter and tries to scout the 4 waypoints before time is up.

In the "Base Defense" scenario, all players are on the rebel side trying to destroy the slowly advancing enemy destroyer using the rebel cruiser.  Periodically enemy frigates will appear and assault your base.  Destroying them drops colored dots - upgrades for your ship.

There's no clean way to shut down the server, just ctrl-c the process.

