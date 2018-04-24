warp
====

Coop Networked Game in Racket

Depends on mode-lambda: raco pkg install mode-lambda

Start a server: racket server.rkt

Start each client: racket client.rkt

This is a coop game!  It is playable with a single person but very difficult!  All players on a ship have the same controls.  More players make your ship more powerful but you have to coordinate!

The game is primarily mouse driven with keyboard shortcuts.  There are a few extra keyboard shortcuts:
- f toggles fullscreen
- tab toggles showing more information on screen
- backtick toggles showing the whole map
- r/t zoom in/out along with mouse wheel
- right-mouse drag moves the view independent of your ship


status
----

Playable but still in development.

In the "Pilot Training" scenario, each player gets their own fighter and tries to scout the 4 waypoints before time is up.

In the "Base Defense" scenario, all players are on the rebel side trying to destroy the slowly advancing enemy destroyer using the rebel cruiser.  Periodically enemy frigates will appear and assault your base.  Destroying them drops colored dots - upgrades for your ship.

In "Asteroid Search", all players are stranded on a ship whose engines have failed.  Use the fighters to scout the asteroids, find the hidden base, return the engine parts to your ship, and finally fly across the field to blow up the Rebel base.

There's no clean way to shut down the server, just ctrl-c the process.

