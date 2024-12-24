# rack-botc

## Installation:

Using Haskell Stack, simply `stack build`, then `stack ghci` to open the project in the interpreter.
There is no custom CLI at the moment.

## Use

Once in the interpreter, load a script with the `loadScript` function, for example
```
> tb <- loadScript "resources/scripts/troublebrewing.json"
```

Then produce a randomized rack with the `rack` function, which needs a script and
a player count like so:
```
> rack tb 9
Rack:   butler, recluse, imp, undertaker, soldier, baron, chef, investigator, saint
Pretenders:
  drunk: sees chef
Bluffs: fortuneteller, empath, ravenkeeper, monk, virgin, slayer
```

Note that the Chef in this rack is actually the Drunk!
The Lunatic and the Marionette function similarly, but their replacements are
only suggestions.

The rack appears in a random order which you can distribute to already-seated
players. If the rack contains a Marionette, you should ensure that one of the
good characters next to the demon is made the Marionette, regardless of the
character suggested by the rack. Similarly you may have to shuffle some seating
if the demon is a Lord of Typhon. For other purposes, it should work.

You may need to show not-in-play characters for various reasons, e.g.
demon bluffs, snitch bluffs, vortoxed pixie, etc. For these purposes,
several bluffs are suggested by the racking tool so that you can safely use
them without confirming that they are not in play. They are, of course,
just suggestions.

## Notes

There is currently no way to adjust the random distribution(s) used to
make choices. Of particular note is the Summoner, which is classed as a
_demon_ by the tool. This means that on a script with four demons and a
summoner, roughly 20% of racks will begin with each demon or with a summoner.
This might be different than expected!

