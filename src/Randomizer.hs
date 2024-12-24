{-# LANGUAGE LambdaCase #-}
module Randomizer (module Randomizer) where

import Control.Monad.Loops (whileJust_)
import Data.Maybe (fromJust)
import Data.Functor (($>))
import Data.List (intercalate)
import Lens.Micro
import Lens.Micro.Mtl

import Character hiding (id)
import Character qualified as C
import Monad
import Script

rack :: Script -> PlayerCount -> IO ()
rack s pc = runRandomizer s pc >>= \case
  Left err -> putStrLn err
  Right r  -> putStrLn $ prettyRack r

data Rack = Rack
  { fables, roles :: [String]
  , pretenders :: [(String, String)]
  , someBluffs :: [String]
  }

prettyRack :: Rack -> String
prettyRack Rack{fables,roles,pretenders,someBluffs} =
  unlines $ concat [fablesLine,rackLine,pretenderLines,bluffsLine]
  where
    fablesLine = ["Fables: " ++ intercalate ", " fables | not (null fables)]
    rackLine   = ["Rack:   " ++ intercalate ", " roles ]
    bluffsLine = ["Bluffs: " ++ intercalate ", " someBluffs]
    pretenderLines = case pretenders of
      [] -> []
      _ -> "Pretenders:" : map (("  "++).pretenderLine) pretenders
    pretenderLine ("drunk",r)      = "drunk: sees " ++ r
    pretenderLine ("marionette",r) = "marionette: replaced by " ++ r
    pretenderLine ("lunatic",r)    = "lunatic: suggest showing " ++ r
    pretenderLine (_,_) = error "Unknown Pretender!"

runRandomizer :: Script -> PlayerCount -> IO (Either String Rack)
runRandomizer s pc = do
  chars <- loadAllCharacters
  runR chars s pc randomizer -- randomizer

randomizer :: R Rack
randomizer = do
  { initialShuffle
  ; tyCounts <- baseTypeCounts <$> use playerCount
  ; drawCharacters tyCounts
  ; forM_ [Fable, Demon, Minion, Townsfolk, Outsider] $ \cty ->
      processAll cty
  ; fables <- allFabledChars
  ; chars  <- allRackedChars
  ; unused <- allReserveGoodChars
  ; buildFinalRack fables chars unused
  }

-- Useful for shuffling
(%%=) :: MonadState s m => Lens' s a -> (a -> m a) -> m ()
(%%=) l f = use l >>= f >>= (l .=)
infix 4 %%=

-- Shuffle each of the reserve lists.
initialShuffle :: R ()
initialShuffle =
  forM_ nonFableTypes $ \cty ->
    ctyLens cty.reserve %%= shuffle

data TypeCounts = TC { countT, countO, countM, countD :: Int }

baseTypeCounts :: PlayerCount -> TypeCounts
baseTypeCounts pc = TC t o m d
  where
    -- key player counts, where the count of minions changes.
    keyCounts = [(13, 3), (10, 2), (7, 1), (5, 1)]
    -- the key count for the actual player count (first keyCount at most pc)
    (pcKey, m) = head $ filter ((<= pc) . fst) keyCounts
    -- there is always (exactly) one demon
    d = 1
    -- outsiders are determined by difference between pc and the key
    o = pc - pcKey
    -- townsfolk fill the remainder
    t = pc - (o + m + d)

drawCharacters :: TypeCounts -> R ()
drawCharacters TC{countT, countO, countM, countD} = do
  -- move all fabled from reserve to unprocessed
  (fabled.reserve <<.= []) >>= (fabled.unprocessed .=)
  -- set the outsider count to the correct initial value
  outsiderCount .= countO
  -- For each type, draw the appropriate number of characters
  forM_ types $ \(cty,count) ->
    replicateM_ count $ cty +@@* unprocessed
  where
    types = [ (Townsfolk,countT)
            , (Outsider,countO)
            , (Minion,countM)
            , (Demon,countD)
            ]

allRackedOf :: CharacterType -> R [Character]
allRackedOf cty = concat <$> traverse (use.(ctyLens cty.)) [unstable,stable]

allFabledChars, allRackedChars, allReserveGoodChars :: R [Character]
allFabledChars = allRackedOf Fable
allRackedChars = concat <$> mapM allRackedOf nonFableTypes
allReserveGoodChars =
  concat <$> traverse (\cty -> use (ctyLens cty.reserve)) [Townsfolk, Outsider]

buildPretenders :: R [(String, String)]
buildPretenders = map build <$> use pretend where
  build (Character{C.id=nameP}, Character{C.id=nameR}) = (nameP,nameR)

buildFinalRack :: [Character] -> [Character] -> [Character] -> R Rack
buildFinalRack fables racked bluffs = do
  let fables' = map C.id fables
  bluffs' <- take 6 <$> shuffle (map C.id bluffs)
  roles <- shuffle (map C.id racked)
  pretenders <- buildPretenders
  return $ Rack {fables=fables', roles, pretenders, someBluffs=bluffs'}

ctyLens :: CharacterType -> Lens' RackState OneTypeRack
ctyLens Fable     = fabled
ctyLens Townsfolk = townsfolk
ctyLens Outsider  = outsiders
ctyLens Minion    = minions
ctyLens Demon     = demons

-- I'm pretty sure this was defined in Lens, but I guess not in microlens?
(<+=) :: (MonadState s m, Num a) => LensLike ((,) a) s s a a -> a -> m a
l <+= x = l <%= (+x)
infix 4 <+=

-- We use @@ in operators to denote doing something to characters,
-- because it looks a little bit like a face.
-- @@* represents a type instead.

infix 4 +@@
infix 4 +@@*
infix 4 -@@*

-- Add the given character to the specified list for its type.
(+@@) :: Character -> ASetter' OneTypeRack [Character] -> R ()
c +@@ l = ctyLens (ctype c) . l %= (c:)

-- Add the first reserve character of the given type to the specified list.
(+@@*) :: CharacterType -> ASetter' OneTypeRack [Character] -> R ()
cty +@@* l = withErr msg $ cty -@@* reserve >>= (+@@ l)
  where msg = "Failed to draw " ++ show cty ++ " character"

-- Remove the first character from the specified list of the given type.
(-@@*) :: CharacterType -> Lens' OneTypeRack [Character] -> R Character
cty -@@* l = do
  list <- use l'
  case list of
    [] -> throwError $ "Failed to remove " ++ show cty ++ " character"
    (c:cs) -> (l' .= cs) $> c
  where
    l' :: Lens' RackState [Character]
    l' = ctyLens cty.l

-- Remove and return the victim character of the given type.
removeVictim :: CharacterType -> R Character
removeVictim cty = (cty -@@* unprocessed) `catchError` (\_ ->
                    cty -@@* unstable)

-- Remove the victim character of the given type, returning it to the
-- reserve list.
removeVictim_ :: CharacterType -> R ()
removeVictim_ cty = removeVictim cty >>= (+@@ reserve)

countSelected :: CharacterType -> R Int
countSelected cty = do
  OTR{_unprocessed,_stable,_unstable} <- use (ctyLens cty)
  return $ length $ _stable ++ _unstable ++ _unprocessed

-- Return all characters of the given type from the selection to the reserve.
deselectAll :: CharacterType -> R ()
deselectAll cty = do
  unproc <- ctyLens cty.unprocessed <<.= []
  unstab <- ctyLens cty.   unstable <<.= []
  stab   <- ctyLens cty.     stable <<.= []
  ctyLens cty.reserve %= ((unproc ++ unstab ++ stab) ++)
  when (cty == Outsider) $ outsiderCount %= min 0

processAll :: CharacterType -> R ()
processAll cty =
  whileJust_ (preuse (ctyLens cty.unprocessed.traverse)) $ \_ -> do
    theChar <- cty -@@* unprocessed -- succeeds by loop condition
    processChar theChar `catchError` charProcessingFailed theChar
  where
    -- character has been removed from the unprocessed list but
    -- all other stateful effects of attempting processing have
    -- been rolled back. Try a different character instead.
    charProcessingFailed :: Character -> String -> R ()
    charProcessingFailed c e = do
      cty +@@* unprocessed -- draw a replacement
      -- add failed char back to end of reserve. This ensures that we
      -- don't lose track of it (e.g. as a potential bluff) but also
      -- ensures that we don't immediately redraw it if there's another
      -- failure just for it to fail again.
      ctyLens cty.reserve %= (++ [c])
      Randomizer $ liftIO $ do
        putStrLn "Drawing a new character because of a failure:"
        putStrLn $ "  " ++ e

makeStable,makeUnstable :: Character -> R ()
makeStable c@Character{ctype}   = ctyLens ctype.  stable %= (c:)
makeUnstable c@Character{ctype} = ctyLens ctype.unstable %= (c:)

processChar :: Character -> R ()
processChar c@Character{setup} = do
  setCurrent c
  processSetup setup >>= \case
    [] -> currentDone >>= maybe (pure ()) makeUnstable
    _  -> currentDone >>= maybe (pure ()) makeStable

-- Process a 'Setup' and all of the changes it implies, returning a list
-- of the individual SetupChanges that were applied.
-- 'NoChange' is never considered "applied."
processSetup :: Maybe Setup -> R [SetupChange]
processSetup Nothing = pure []
processSetup (Just s) = processSetup' s

processSetup' :: Setup -> R [SetupChange]
processSetup' (SetupChange sc) = processSetupChange sc
processSetup' (SetupAny choices) = choose choices >>= processSetup'
processSetup' (SetupAll ss) = fmap concat (mapM processSetup' ss)

-- Process a single 'SetupChange'. Return the empty list if the change
-- was 'NoChange', otherwise return the change in a singleton list.
-- If this function fails, it will be caught in 'processAll' and we will
-- simply replace the offending character and try again.
-- If no replacements are possible, an error will be raised indicating
-- that it was not possible to draw another character.
processSetupChange :: SetupChange -> R [SetupChange]
processSetupChange NoChange = pure []
processSetupChange other = processSetupChange' other $> [other]

processSetupChange' :: SetupChange -> R ()
processSetupChange' NoChange = error "NoChange survived processSetupChange?"
processSetupChange' (AddOutsiders n) = replicateM_ n addOutsider
processSetupChange' (SubOutsiders n) = replicateM_ n subOutsider
processSetupChange' (AddCharacter n name) = addCharacter n name
processSetupChange' Pretend = processPretend
processSetupChange' SetupLegion = setupLegion
processSetupChange' SetupActor = setupActor
processSetupChange' ArbOutsiders = makeOutsidersArbitrary
processSetupChange' NoDemon = setupNoDemon
processSetupChange' NoMinions = setupNoMinions
processSetupChange' (AddMinion n) = addMinion n
processSetupChange' (SubMinion n) = subMinion n

unlessOutsidersAreArbitrary :: R () -> R ()
unlessOutsidersAreArbitrary r = do
  arb <- use arbitraryOutsiders
  unless arb r

-- Add one outsider.
--  To add one outsider, increment the outsider count. If the count is
--  now positive, remove a victim townsfolk and add a reserve outsider.
addOutsider :: R ()
addOutsider = unlessOutsidersAreArbitrary $ do
  newCount <- outsiderCount <+= 1
  when (newCount > 0) $ do
    removeVictim_ Townsfolk
    Outsider +@@* unprocessed

-- Sub one outsider.
--  To sub one outsider, decrement the outsider count. If the count is
--  now **not negative**, remove a victim outsider and add a reserve townsfolk.
-- If the count is currently zero or less, there is a 25% chance that this
-- operation is ignored.
-- TODO: ignored operations should not cause stability.
-- TODO: Chance of skipping should be configurable.
subOutsider :: R ()
subOutsider = unlessOutsidersAreArbitrary $ do
  oldCount <- use outsiderCount
  if oldCount <= 0
  then do p <- getRandomR (0, 1 :: Double)
          when (p > 0.25) go
  else go
 where
  go = do
    newCount <- outsiderCount <+= -1
    when (newCount >= 0) $ do
      removeVictim_ Outsider
      Townsfolk +@@* unprocessed

-- AddCharacter: Fail if the character is a minion or demon, no support
--  for that at the moment.
--  Find the named character and remove it from any lists it might be in.
--  If it was already selected (unprocessed, unstable, or stable) then it
--  can be dropped directly into the stable rack.
--  Otherwise, if it's a townsfolk, then the appropriate
--  number of victims are removed and the appropriate number of the specified
--  townsfolk are added to the STABLE rack.
--  If N copies of an outsider are to be generated, then a random integer R
--  in the range [0,min(N,#victim outsiders)] is generated.
--  R victim outsiders are removed, and (N-R) victim townsfolk are removed.
--  Then N copies of the specified outsider are added to the STABLE rack.
-- If an outsider is added, the outsider count in the state is clamped to
-- zero from below before being incremented, in case it was previously
-- negative.
addCharacter :: Int -> String -> R ()
addCharacter n name = do
  r  <- findCharacter name
  mc <- removeFoundCharacter r
  c  <- case mc of
    -- didn't find the character: it better be the current character!
    Nothing -> use current >>= assertNamed name . fromJust
    Just c' -> pure c'
  when (ctype c `elem` [Minion,Demon]) $ error "AddCharacter minion or demon?"
  case r of
    Selected cty c' -> ctyLens cty.stable %= (c':)
    Reserve _ c'    -> addCharacterRecord n c'
    -- In this case, the character is necessarily the current processee.
    -- It'll be added to the stable rack when we're done anyway in the normal
    -- course of operation. But we must also add the specified number
    -- and remove victims for them immediately, identical to Reserve case.
    NotFound        -> addCharacterRecord n c
  where
    assertNamed expected c@Character{C.id=actual} = do
      when (expected /= actual) $
        error $ "AddCharacter: NotFound but also not current? ("
          ++ expected ++ "," ++ actual ++ ")"
      pure c

data FindCharacterResult
  = Reserve CharacterType Character
  | Selected CharacterType Character
  | NotFound

findCharacter :: String -> R FindCharacterResult
findCharacter name = finalize <$> search where
  search :: R [FindCharacterResult]
  search = concat <$> mapM searchType nonFableTypes

  searchType :: CharacterType -> R [FindCharacterResult]
  searchType cty = use (ctyLens cty) <&> searchOTR where
    searchOTR OTR{_reserve,_unprocessed,_unstable,_stable} =
      [ searchList (Reserve cty) _reserve
      , searchList (Selected cty) (_unprocessed ++ _unstable ++ _stable)
      ]

  searchList :: (Character -> FindCharacterResult) -> [Character] -> FindCharacterResult
  searchList _ [] = NotFound
  searchList mk (c@Character{C.id=name'} : cs)
    | name == name' = mk c
    | otherwise = searchList mk cs

  finalize :: [FindCharacterResult] -> FindCharacterResult
  finalize [] = NotFound
  finalize (NotFound : rs) = finalize rs
  finalize (found : _) = found

-- Remove a found character from any lists it might be in.
removeFoundCharacter :: FindCharacterResult -> R (Maybe Character)
removeFoundCharacter NotFound = pure Nothing
removeFoundCharacter other
  | Reserve cty c <- other 
  = go (ctyLens cty.reserve) (C.id c) $> Just c
  | Selected cty c <- other
  = do go (ctyLens cty.unprocessed) $ C.id c
       go (ctyLens cty.unstable) $ C.id c
       go (ctyLens cty.stable)   $ C.id c
       return $ Just c
  where
    go list name = list %= filter (notNamed name)
    notNamed name Character{C.id=name'} = name /= name'

addCharacterRecord :: Int -> Character -> R ()
addCharacterRecord n c = do
  let cty = ctype c
  -- remove appropriate victims
  removeVictimsFor n cty
  -- add n copies of c to the stable rack for its type
  ctyLens cty.stable %= (replicate n c ++)
  -- If cty is Outsider, fix up the outsider count.
  when (cty == Outsider) $ do
    outsiderCount %= max 0 -- ensure at least 0
    outsiderCount += n     -- increment by number added

-- remove victims to satisfy setup modifications of the form
-- [+N CHARACTER] where CHARACTER is a townsfolk or outsider.
removeVictimsFor :: Int -> CharacterType -> R ()
removeVictimsFor n Townsfolk = replicateM_ n $ removeVictim_ Townsfolk
removeVictimsFor n Outsider = do
  outsiderOTR <- use outsiders
  let outsiderVictims = _unprocessed outsiderOTR ++ _unstable outsiderOTR
      outsiderVictimCount = length outsiderVictims
      capOnR = min n outsiderVictimCount
  r <- getRandomR (0, capOnR)
  -- there are definitely at least R victim outsiders available,
  -- so outsiderCount is also currently at least R. No worries here.
  outsiderCount += negate r
  replicateM_ r     $ removeVictim_ Outsider
  replicateM_ (n-r) $ removeVictim_ Townsfolk
removeVictimsFor _ Fable  = pure ()
removeVictimsFor _ Minion = error "How to remove victims for minions?"
removeVictimsFor _ Demon  = error "How to remove victims for demons?"

-- Pretend: Move this character from the stable rack (where it was just placed)
--  to the Pretend rack. Depending on what we are pretending, do the following:
--    Lunatic: Add a demon to the stable rack.
--    Marionette: Add a townsfolk to the unprocessed rack.
--    Drunk: Add a townsfolk to the stable rack.
data Pretender = Drunk | Lunatic | Marionette deriving Eq
processPretend :: R ()
processPretend = do
  mc <- currentDone
  let c@Character{C.id=name} = case mc of
        Just c' -> c'
        Nothing -> error "processPretend: No current character?"
      p = case name of
            "lunatic"    -> Lunatic
            "drunk"      -> Drunk
            "marionette" -> Marionette
            other -> error $ "processPretend: unknown pretender " ++ other
      (rty,list) = fromJust $ lookup p control
  r <- rty -@@* reserve
  pretend %= ((c,r):)
  r +@@ list
  where
    control :: [(Pretender, (CharacterType, ASetter' OneTypeRack [Character]))]
    control =
      [ (Drunk, (Townsfolk, stable))
      , (Lunatic, (Demon, stable))
      , (Marionette, (Townsfolk, unprocessed))
      ]

setupLegion :: R ()
setupLegion = do
  pc <- use playerCount
  let legionCount = 3*pc `div` 4
  -- remove minions
  deselectAll Minion
  -- insert remaining legions
  legion <- fromJust <$> use current
  demons.stable %= (replicate (legionCount-1) legion ++)
  -- set outsiders to somewhere between 0 and normal
  oc <- use outsiderCount
  r  <- getRandomR (0, oc) -- pick number of outsiders to remain
  replicateM_ (oc-r) $ removeVictim_ Outsider
  outsiderCount .= r
  -- fixup townsfolk
  let numTownNeeded = pc - (legionCount + r)
  numTownSelected <- countSelected Townsfolk
  let numTownDelete = numTownSelected - numTownNeeded
  replicateM_ numTownDelete $ removeVictim_ Townsfolk

setupActor :: R ()
setupActor = do
  -- calculate correct number of good characters
  pc <- use playerCount
  demonCount  <- countSelected Demon -- not always 1: legion!
  minionCount <- countSelected Minion
  let goodCount = pc - (demonCount + minionCount)
  -- remove all good characters
  deselectAll Townsfolk
  deselectAll Outsider
  outsiderCount .= 0
  -- insert remaining actors
  actor <- fromJust <$> use current
  townsfolk.stable %= (replicate (goodCount-1) actor ++)

-- Remove all outsiders. Then, select a random number
--  in the range [0,OutsidersOnScript]. Add that many Outsiders
--  to the unstable rack. Add or remove (victim) Townsfolk as necessary
--  until the correct number of characters are in the rack.
--  Set a flag that the outsider count is arbitrary. Further numerical
--  changes to the outsider rack are ignored.
--  AddCharacter changes that add outsiders are still performed.
--  Note: Minions and Demons must be included in the character count,
--  as Kazali and and Lord of Typhon change the count of evil players.
makeOutsidersArbitrary :: R ()
makeOutsidersArbitrary = do
  -- remove all outsiders
  deselectAll Outsider
  -- how many outsiders to include?
  -- TODO: This should probably be better configurable in the setup
  -- control in the character JSON. Xaan really wants in range [1,4]
  -- while Kazali and Lord of Typhon want [0,BaseOutsiderCount] (?).
  numOutsidersOnScript <- length <$> use (outsiders.reserve)
  numOutsidersToUse <- getRandomR (0, numOutsidersOnScript)
  replicateM_ numOutsidersToUse $ Outsider +@@* unprocessed
  outsiderCount .= numOutsidersToUse
  -- how many townsfolk should be in the rack?
  -- We can't just compute old outsider count less new count, because
  -- this action is used by some characters who need to fix the player
  -- count after doing something very destructive like removing
  -- the whole evil team. Instead, we calculate from scratch how
  -- many townsfolk are needed to finish the rack and then add or
  -- remove townsfolk as needed to get to the right number.
  pc <- use playerCount
  demonCount  <- countSelected Demon
  minionCount <- countSelected Minion
      -- add an additional 1 to nonTownCount for the character currently
      -- being processed! Even if the current character is Town, it will
      -- definitely be added back when we're done, so don't try to replace it.
  let nonTownCount = demonCount + minionCount + numOutsidersToUse + 1
      needTownCount = pc - nonTownCount
  currentTownCount <- countSelected Townsfolk
  let diff = needTownCount - currentTownCount
  case diff `compare` 0 of
    -- negative: we have too many town
    LT -> replicateM_ (negate diff) $ removeVictim_ Townsfolk
    -- zero: nothing to do!
    EQ -> pure ()
    -- positive: we need more town
    GT -> replicateM_ diff $ Townsfolk +@@* unprocessed
  -- Set the flag indicating we've performed this operation.
  arbitraryOutsiders .= True

-- Remove all demons from the setup.
-- Currently, characters that do this are responsible for fixing
-- the character count in some other way:
--   Lil' Monsta: Adds a minion.
--   Atheist: Runs makeOutsidersArbitrary
-- However in the future, a more robust implementation _may_ replace
-- the demon(s) with townsfolk.
setupNoDemon :: R ()
setupNoDemon = do
  -- If the character we're processing is the demon, don't add it back
  -- to the rack when we're done.
  mc <- use current
  let currentIsDemon = maybe False ((Demon ==) . ctype) mc
  when currentIsDemon $ void currentDone
  -- Then remove all demons.
  deselectAll Demon

setupNoMinions :: R ()
setupNoMinions = do
  -- How many minions are about to be removed?
  count <- countSelected Minion
  -- Remove them all.
  deselectAll Minion
  -- Add that many extra townsfolk.
  replicateM_ count $ Townsfolk +@@* unprocessed

addMinion :: Int -> R ()
addMinion n = replicateM_ n $ Minion +@@* unprocessed

subMinion :: Int -> R ()
subMinion n = replicateM_ n $ do
  removeVictim_ Minion
  Townsfolk +@@* unprocessed
