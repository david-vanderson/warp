warp
====

Coop Networked Game in Racket

Start a server: racket sandbox.rkt

Start each client: racket client.rkt

The game is all mouse driven.  Try clicking "Crewer on Rebel Base", then "Weapons".  Once in the weapons station (check the text at the top middle of the screen), any click in front of the ship should shoot, if you have enough energy.  The "Observer" station can't do anything, but you can see the energy of all the stations on the ship.

This is a coop game!  It is very difficult to play with just a single person.  You'll generally want at least one person to pilot a ship and another person to sit at weapons.  To make this a bit easier, the AI controls any station that a player is not occupying.


status
----

Still in development, but playable.  Try to launch a Rebel Frigate off the Rebel Base and destroy the Empire Base (off to the right).  Your frigates don't have any AI.  Every few minutes some more enemy frigates will appear and attack your base.

There is no win/loss screen.  A message will be displayed whenever a ship (or base) is destroyed, but the game keeps going forever.


