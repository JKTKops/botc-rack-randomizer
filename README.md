# rack-botc

## Installation:

Using Haskell Stack, simply `stack install`
There is only an extremely simple CLI at the moment.

For the more powerful methods of interacting with the tool, such as rapid script experimentation
or testing interactions of setup clauses on custom characters, then you want to enter the
Haskell interpreter. `stack build`, then `stack ghci` to open the project in the interpreter.

## Use

### CLI

After `stack install`, run `rack-botc SCRIPTFILE PC`, where SCRIPTFILE is the
path to a `.json` file containing the script and PC is the number of players.
A rack and some suggested bluffs will be output.

All non-fabled official characters are implemented.
The tool does not understand most fabled characters, but it understands the ones
that commonly appear in script `.json` files: Bootlegger, Sentinel, and SOI.
Others are easy to add but this was a weekend project and it's low priority ;)

### Interpreter

After `stack build`, run `stack ghci` to launch the interpreter.
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
several bluffs are suggested by the racking tool that you can safely use.
They are, of course, just suggestions.

The `rack` function expects a `Script`, which is a `[Character]`. You can implement
your own custom `Character`s in the interpreter -- use `:info Character` to see what
fields are required and `:info` on the types of the fields to see how to form them.
If you don't know Haskell, I don't recommend this use case of the tool.

## Notes

There is currently no way to adjust the random distribution(s) used to
make choices. Of particular note is the Summoner, which is classed as a
_demon_ by the tool. This means that on a script with four demons and a
summoner, roughly 20% of racks will begin with each demon or with a summoner.
This might be different than expected!

## Custom Character Support

Any character whose setup clauses can be expressed in terms of the primitive
setup modifications "+/- outsider(s)", "+ (some number of) specific character",
"you think you are," "+? to -? outsiders", "no demon", "no minions",
and "+/- minion(s)", and either random choice between those or several of those
simultaneously, can be expressed within the randomizer's framework.

There are two additional primitives, "legion" and "actor."

The "legion" primitive sets up a Legion game using the recommended number of
Legion players, which is the number of good players that would be required
if it weren't a Legion game. The number of Legions is **not** random.
The primitive does not insert Legions explicitly, rather, it inserts copies
of whatever character triggered the primitive. However, that character is assumed
to be a Demon.

The "actor" primitive is used for a custom character that my group enjoys.
All good players are replaced by the character that triggered the primitive,
which is assumed to be a Townsfolk.

## Future Work

It is theoretically possible to use the same algorithm that this tool uses to
generate random racks to test the validity of custom racks. Essentially instead
of shuffling the reserve at the start, you simply make the head of each reserve
be the characters in the input rack, and check that the output rack is equal to
the input rack. The catch is that some setup clauses, when randomized, require
random decisions (randomization is not just choice of characters!). Additional
code and probably an operating modality would be required to search through
such choices to find a set of choices that leads to the right rack. This is
still much easier than searching through _all_ racks, and is totally feasible,
but it's not implemented.

