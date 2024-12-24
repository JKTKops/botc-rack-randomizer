{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-
Define the Character record, representing a single BotC character
for the purposes of racking.

Each character has an id to uniquely identify it and a field
describing how the character's presence affects setup.
-}

module Character (module Character) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (unpack)
import Data.Vector (toList)

data CharacterType = Fable | Townsfolk | Outsider | Minion | Demon
  deriving (Eq, Ord, Show, Read)

allCharacterTypes, nonFableTypes :: [CharacterType]
allCharacterTypes = [Fable, Townsfolk, Outsider, Minion, Demon]
nonFableTypes = tail allCharacterTypes

data SetupChange
  -- If the following modifications are present in the
  -- setup field of a character, they are assumed disjunctive
  = NoChange         -- Do nothing. Required for Balloonist and VI.
  | AddOutsiders Int -- [+N outsider]
  | SubOutsiders Int -- [-N outsider]
  | AddCharacter Int String -- [+N Character] (Huntsman, Choirboy, VI)
      -- Drunk, Lunatic, Mario
  | Pretend -- You think you are ... [pretend]
            -- These all behave somewhat differently so we just
            -- handle them all individually. A bit unfortunate.
  -- If any of the following modifications are present in the
  -- setup field of a character, they are assumed conjunctive
  | SetupLegion      -- [most players are Legion]
  | SetupActor       -- [all Good are Actors]
  | ArbOutsiders     -- [? outsider], e.g. Kazali, Xaan
      -- since these are conjunctive, [NoDemon,NoMinions] works for atheist.
  | NoDemon          -- [no demon] e.g. atheist
  | NoMinions        -- [no minions] e.g. Kazali
  | AddMinion Int    -- [+N minion] e.g. Lord of Typhon, Lil' Monsta
  | SubMinion Int    -- [-N minion] used to implement summoner
  deriving Show

-- [Note: Lord of Typhon and Lil' Monsta Minions]
-- The [+1 minion] from Lord of Typhon and from Lil' Monsta
-- behave differently: one replaces a demon, and the other would replace
-- a townsfolk (except that outsider count is arbitrary, so it actually
-- might replace an outsider). We rely on the arbitrary outsider count
-- SetupChange to fix this: Lil' Monsta removes itself and adds a minion,
-- all is good; Lord of Typhon adds a minion, then ArbOutsiders
-- fixes the count of townsfolk.

-- [Note: Summoner Character Type]
-- Summoner is currently considered a Demon, rather than a Minion.
-- This might cause the randomizer to suggest that you show a Lunatic
-- the Summoner token, which you obviously cannot do.
-- The choice to treat as a demon simplifies the handling of the Summoner
-- and makes it significantly less of a special case than ideas for
-- handling it as a minion. The result is hopefully a smoother
-- component integration making it easier to add new characters in the future.

-- [Note: Bluffs]
-- Part of a rack is the bluffs (usually for the demon, but sometimes
-- for minions if there's a Snitch or Lord of Typhon). The bluffs the
-- user shows are entirely up to them. If we create a REPL-style interface
-- to the randomizer, then we can easily add a feature to generate bluffs,
-- but for now it's out of scope.

-- [Note: Algorithm]
-- We maintain 4 lists for each character type: the reserve, the unprocessed
-- rack, the stable rack, and the unstable rack.
-- There is a single additional list shared by all types,
-- called the pretend rack. The Drunk, Lunatic, and Marionette all
-- appear on the pretend rack along with the character that was added
-- to replace them. Lil' Monsta also appears on the Pretend rack if in play.
-- A couple of notes on the Pretend feature:
--   1. The Marionette's pretend character is just logging information.
--      If a good character neighbours the demon after assignment,
--      make them the Marionette. Otherwise, swap one of the nighbouring
--      minions with the first good token in the rack
--      (to retain randomness). The Balloonist jinx ensures this works.
--   2. The Lunatic's pretend character is a suggestion. Show the real
--      demon if you prefer. The only way that Lil' Monsta can appear in
--      in the final rack is if the Lunatic is pretending to be it,
--      but you are not allowed to show a Lunatic the Lil' Monsta,
--      so if this occurs, you'll have to pick a different demon.
--      Of course, it is up to you which players see demon or Lunatic
--      tokens, but the racked demon which is not the Lunatic suggestion
--      must be the real demon as it may have affected setup.
--   3. The Drunk's pretend character is not a suggestion:
--      unlike Mario, a Drunk townsfolk does not affect setup,
--      and making a different townsfolk Drunk instead can invalidate
--      the rack.
--     
--
-- We start by randomly shuffling the lists of available characters of
-- each type.
-- Our initial rack takes the appropriate number of characters
-- from each shuffle, creating the unprocessed rack for each type,
-- and leaves the rest in "reserve."
-- All Fables on the script are put on the unprocessed rack,
-- and are not shuffled.
-- In the order:
--    Fables, Demons, Minions, Townsfolk, Outsiders
-- we process -- each character in the unprocessed rack,
-- in the order they were selected.
-- This order guarantees that a setup modification in each
-- stage will not add new characters from previous stages, ensuring
-- termination. We could work around this with a fixpoint algorithm,
-- but that is not currently necessary. BEWARE if that changes.
--   Specifically, demons can add minions (typhon, monsta) and
--   minions can add or remove townsfolk (godfather),
--   but townsfolk/outsiders cannot affect evils
--   (except to remove them all). There is tension between Summoner and Kazali,
--   and Summoner is just generally ugly to deal with as a minion. So instead
--   we treat Summoner as an extra Demon, and process it with the Demons.
--   So first we process the demon, in case this modifies minions,
--   then process minions.
--   Removing characters of a type already processed is not generally
--   safe as that character may have applied setup modifications already.
--   With this order, the only removal that can happen "out of order"
--   is an Atheist removing the evil team. Luckily for us, setup modifications
--   already applied in evil stages are irrelevant if there's an Atheist.
--   We might as well output what the evil team was _going_ to be,
--   but we can safely replace it with whatever we want. Arguably,
--   we can safely output the single character "Atheist" because Atheist
--   racks should be built by hand in most cases IMO.
-- For each character in the unprocessed rack, one of two things happens:
--   1. The character does not cause setup modifications.
--      We move it to the unstable rack and continue.
--   2. The character causes setup modifications.
--      Apply the setup modification, then move the character to the
--      stable rack. The setup modification might delete the character
--      (c.f. Lil' Monsta) in which case we don't add it to the stable rack.
--      Stable characters cannot be removed later (except by Atheist)
--      ensuring that performed setup modifications are always justified.
--
-- Modification terminology:
--   A) The term "victim [type]" refers to the first unprocessed
--      character of that type, or the first unstable if none are
--      unprocessed. If there is no victim of a type, but one must
--      be removed, a warning is signalled and we fail to generate a rack.
--      (do-over logic can be implemented at the top level.)
--      Atheist will be detected and handled specially rather than failing.
--   B) "#victim [type]" refers to the number of "victim [type]" that
--      could be removed before failure; that is,
--      len(unprocessed)+len(unstable).
--   B) To "remove" a character means to take it from its current list
--      and put it back on the reserve list.
--   C) To "add" a type means to take the first character on the
--      on that type's reserve list and add it to the unprocessed list.
--
-- NoChange: Do nothing.
-- AddOutsiders: Add an Outsider the appropriate number of times.
--  To add one outsider, increment the outsider count. If the count is
--  now positive, remove a victim townsfolk and add a reserve outsider.
-- SubOutsiders: Sub an Outsider the appropriate number of times.
--  To sub one outsider, decrement the outsider count. If the count is
--  now **not negative**, remove a victim outsider and add a reserve townsfolk.
-- AddCharacter: Fail if the character is a minion or demon, no support
--  for that at the moment.
--  Unless the character to be added is also the character that caused this
--  SetupChange to be applied, if the character is already in the rack,
--  ensure that it is in the STABLE rack but otherwise make no changes.
--  Otherwise, if it's a townsfolk, then the appropriate
--  number of victims are removed and the appropriate number of the specified
--  townsfolk are added to the STABLE rack.
--  If N copies of an outsider are to be generated, then a random integer R
--  in the range [0,min(N,#victim outsiders)] is generated.
--  R victim outsiders are removed, and (N-R) victim townsfolk are removed.
--  Then N copies of the specified outsider are added to the STABLE rack.
-- Pretend: Move this character from the stable rack (where it was just placed)
--  to the Pretend rack. Depending on what we are pretending, do the following:
--    Lunatic: Add a demon to the stable rack.
--    Marionette: Add a townsfolk to the unprocessed rack.
--    Drunk: Add a townsfolk to the stable rack.
-- SetupLegion: Calculate how many players should be Legion for the current
--  player count. I think (3/4)*P rounded down feels about right.
--  Pick a random number in the range [0, BaseOutsiderCount].
--  Remove all but that many victim Outsiders. Calculate how many Townsfolk
--  are needed to complete the rack. Remove all but that many victim Townsfolk.
--  Warning: on scripts with Townsfolk that can affect team balance, like the
--  Ballonist and the Bounty Hunter, random Legion games are risky.
-- SetupActor: Remove all good characters, and add the matching number of
--  Actors.
--  Warning: Actor and Atheist are obviously incompatible within the rules;
--  it is probably a bad idea to randomize racks on a script with both.
--  You may end up with a rack containing only Actors, for example.
-- ArbOutsiders: Remove all outsiders. Then, select a random number
--  in the range [0,OutsidersOnScript]. Add that many Outsiders
--  to the unstable rack. Add or remove (victim) Townsfolk as necessary
--  until the correct number of characters are in the rack.
--  Set a flag that the outsider count is arbitrary. Further numerical
--  changes to the outsider rack are ignored.
--  AddCharacter changes that add outsiders are still performed.
--  Note: Minions and Demons must be included in the character count,
--  as Kazali and and Lord of Typhon change the count of evil players.
-- NoDemon: cases based on what we are processing:
--  Lil' Monsta: remove it from the stable rack.
--  Atheist: Empty the stable and unstable Demon racks.
--  Otherwise: remove the victim Demon, add a Townsfolk.
--  The 'otherwise' case should never be hit with current characters,
--  so it's safer for now to make it a failure condition.
-- NoMinions: Remove all Minions. Add that many Townsfolk.
--  Only Kazali and Atheist currently do this, and both immediately follow
--  it up with ArbOutsiders.
--  When Kazali is applied, all Minions should be unprocessed;
--  this can be asserted in the code as a safety check.
--  When Atheist is applied, they should all be processed, and as a special
--  case we remove them even if they are not victims.
-- AddMinion: Add a minion. Do not touch other character counts.
--  Lil' Monsta corrects the character count by removing itself.
--  Lord of Typhon corrects the character count with ArbOutsiders.
--  Note that Summoner is considered a Demon, and therefore cannot be added
--  by this modification.
-- SubMinion: Remove a Minion and add a Townsfolk.
--  Summoner uses this in order to functionally replace the demon.

-- Currently any well-formed @Setup is an All of Any of Changes,
-- or possibly skipping a layer. Regardless, we support any layering
-- by recursive application so do not rely on this assumption.
data Setup
  = SetupChange SetupChange
  | SetupAny [Setup]
  | SetupAll [Setup]
  deriving Show

disjunctiveChanges :: [SetupChange] -> [SetupChange]
disjunctiveChanges = filter disj where
  disj NoChange{} = True
  disj AddOutsiders{} = True
  disj SubOutsiders{} = True
  disj AddCharacter{} = True
  disj Pretend{} = True

  disj SetupLegion  = False
  disj SetupActor   = False
  disj ArbOutsiders = False
  disj NoDemon      = False
  disj NoMinions    = False
  disj AddMinion{}  = False
  disj SubMinion{}  = False

conjunctiveChanges :: [SetupChange] -> [SetupChange]
conjunctiveChanges = filter conj where
  conj SetupLegion  = True
  conj SetupActor   = True
  conj ArbOutsiders = True
  conj NoDemon      = True
  conj NoMinions    = True
  conj AddMinion{}  = True
  conj SubMinion{}  = True

  conj NoChange{} = False
  conj AddOutsiders{} = False
  conj SubOutsiders{} = False
  conj AddCharacter{} = False
  conj Pretend{} = False

structureChanges :: [SetupChange] -> Setup
structureChanges cs = build (disjs cs, conjs cs)
  where
    disjs = map SetupChange . disjunctiveChanges
    conjs = map SetupChange . conjunctiveChanges
    build ([], cs') = SetupAll cs'
    build (ds, []) = SetupAny ds
    build (_ds, _cs) =
      error "A character has both conjunctive and disjunctive setup modifications?"

data Character = Character
  { id :: String
  , setup :: Maybe Setup
  , ctype :: CharacterType
  }
  deriving (Show)

parseCharacterType :: String -> Parser CharacterType
parseCharacterType = \case
  "fabled"    -> pure Fable
  "townsfolk" -> pure Townsfolk
  "outsider"  -> pure Outsider
  "minion"    -> pure Minion
  "demon"     -> pure Demon
  invalid     -> fail $ "Parsing CharacterType failed: " ++ invalid

parseSetupChange :: Value -> Parser SetupChange
parseSetupChange = withText "SetupChange" $ \t ->
  case unpack t of
    '-':remove   -> parseRemove remove
    '+':include  -> parseInclude include
    "none"       -> pure NoChange
    "? outsider" -> pure ArbOutsiders
    "legion"     -> pure SetupLegion
    "actor"      -> pure SetupActor
    "no demon"   -> pure NoDemon
    "no minions" -> pure NoMinions
    "pretend"    -> pure Pretend
    invalid -> fail $ "Parsing SetupChange failed: " ++ invalid
  where
    parseRemove s = case reads s of
      [(n, " outsider")] -> pure $ SubOutsiders n
      [(n, " minion")]   -> pure $ SubMinion n
      _invalid -> fail $ "Parsing SetupChange failed: -" ++ s
    parseInclude s = case reads s of
      [(n, " outsider")] -> pure $ AddOutsiders n
      [(n, " minion")]   -> pure $ AddMinion n
      [(n, ' ':name)]    -> pure $ AddCharacter n name
      _invalid -> fail $ "Parsing SetupChange failed: -" ++ s

parseSetup :: [Value] -> Parser Setup
parseSetup = fmap structureChanges . mapM parseSetupChange

instance FromJSON Setup where
  parseJSON = withArray "Setup" $ \v ->
    parseSetup $ toList v

instance FromJSON CharacterType where
  parseJSON :: Value -> Parser CharacterType
  parseJSON = withText "CharacterType" (parseCharacterType . unpack)

instance FromJSON Character where
  parseJSON :: Value -> Parser Character
  parseJSON = withObject "Character" $ \v -> Character
    <$> v .: "id"
    <*> v .:? "setup"
    <*> v .: "type"

loadAllCharacters :: IO [Character]
loadAllCharacters = do
  Just cs <- decodeFileStrict "resources/characters.json"
  return cs
