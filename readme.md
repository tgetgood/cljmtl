# Grand Theft Wumpus

We're going to go through chapter 8 of Conrad Barski's wonderful and eccentric
Land of Lisp.

The code from the book is available [here](http://landoflisp.com/wumpus.lisp),
but we're going to reimplement it --- In real time and under pressure --- so
don't look.

## Plot

You (the Lisp alien) and the Wumpus are partners in crime. You've just come to
Congestion City to make your fortune. But after your first job, knocking off a
liquor store, the Wumpus betrays you, takes the getaway car and the money, and
leaves you high and dry.

You've got to catch the Wumpus and get that money, or you're as good as dead.

The good news is that you managed to wound the Wumpus as he drove off, but the
bad news is that now you have only one bullet left. When you find hum, you'll
need to charge in guns blazing (very briefly) or you're in trouble.

You don't know your way around the city, the cops have set up roadblocks at
random, and worse yet, there's a rival gang in town (the Glowworms) that doesn't
like competiton. At least they glow.

## Walkthrough

### Generate the world

The first thing we need to do is generate congestion city and populate it with
glowworms, roadblocks, and the wumpus itself.

Things to keep in mind:

* Make sure the city is connected
* There should be blood stains on all squares within two of the wumpus
* There's an example of rendering a graph in graph.clj. We can use it for
  testing our generated city

### The Player

Now we have a game world, but not a game. Let's fix that. We need to:

* Place the player when a game starts
* Add commands to walk and charge between nodes
* Describe locations as the player reaches them
* Detect final states (win / lose)
* Track where the player has been
* Add a map command that shows the seen parts of the city

### Game Repl

We could play the game by typing `(walk :east)`, but that's no good. We should
be able to type `walk east`. There's a vanilla REPL implementation in
repl.clj. Extend it to take the kinds of input one expects in a text game.

### Extras

Now we've got a game! But that's no reason to stop. Here are some more ideas to
extend the game, but don't let them limit you.

* Generate names for the city areas
* Roving glowworm gangs
* Time limit
* Multiplayer
* Integrate with [Yetibot](https://yetibot.com/)
