warp
====

Coop Networked Game in Racket

Start a server/scenario: racket base-defense.rkt

Start each client: racket client.rkt

The game is all mouse driven.  Try clicking the gray button "Crewer on Rebel Cruiser", then "Pilot".  Clicking anywhere will turn the ship, and the "Go" button fires the engines.  Get a friend in weapons, where any click within the firing arc will shoot.  Clicking in tactics will shoot shield barriers that block plasma shots.

There is always a "leave" button in the bottom left corner.  Use it to get out of a station.  If you use it while choosing a station, you'll leave that ship - possibly jumping off in a spacesuit.  Just click "leave" again to start over.

This is a coop game!  It is very difficult to play with just a single person.  You'll generally want at least one person to pilot a ship and another person to sit at weapons.  To make this a bit easier, the AI controls any station that a player is not occupying.


status
----

Still in development, but playable.  In the "base-defense.rkt" scenario, your job is to destroy the slowly advancing enemy destroyer using the rebel cruiser.  Periodically enemy frigates will appear and assault your base.  Destroying them drops colored dots - upgrades for your ship.

There is no win/loss screen.  A message will be displayed whenever a ship (or base) is destroyed, but the game keeps going forever.

There's no clean way to shut down the server, just ctrl-c the process.

