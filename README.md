warp
====

Light Multiplyaer Game in Racket for LAN parties.

raco pkg install warp

Start a server: raco warp -s

Start each client: raco warp

Most scenarios are cooperative - it is playable with a single person but very difficult!  All players on a ship have the same controls.  More players make your ship more powerful but you have to coordinate!

The game is primarily mouse driven with keyboard shortcuts.  There are a few extra keyboard shortcuts:
- ctrl-f toggles fullscreen
- ctrl-q asks if you want to quit
- tab toggles showing more information on screen
- backtick toggles showing the whole map
- r/t zoom in/out along with mouse wheel
- right-mouse drag moves the view independent of your ship


status
----

Playable but still in development.

In the "Pilot Training" scenario, each player gets their own fighter and tries to scout the 4 waypoints before time is up while avoiding the asteroids.

In the "Base Defense" scenario, all players are on the rebel side trying to destroy the slowly advancing enemy destroyer using the rebel cruiser.  Periodically enemy frigates will appear and assault your base.  Destroying them drops upgrades for your ship.

In "Asteroid Search", all players are stranded on a ship whose engines have failed.  Deploy probes to scout the asteroids for the hidden base.  Use a fighter to get the engine parts from the base to your ship, and finally fly across the field to blow up the Rebel Outpost.

There's no clean way to shut down the server, just ctrl-c the process.

