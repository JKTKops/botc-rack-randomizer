{-# LANGUAGE TemplateHaskell #-}
{-
The monad underlying the randomization algorithm.
We could use effectful or something like that if we wanted blazing performance
but for a concrete monad stack, MTL does fine.

We need:
  - a MonadRandom for randomization. This will be IO because it's convenient.
    Arbitrary lifting into MonadIO will not be supported.
  - Failure, because in rare cases it might be impossible to satisfy a setup
    modification. For Townsfolk we can simply replace them, but for the evil
    team we can't guarantee that this will work. (This is probably only
    relevant for badly-designed scripts.)
    An ErrorT in our stack will suffice.
  - State, for managing the algorithm. The state record is rather complex
    so we make aggressive use of lenses for manipulating it.
    We want transactional state over errors, so that m `catch` h discards
    state changes made by m. MTL (accidentally?) provides this.
    The StateT transformer is transactional over monads in the stack,
    but not under monads, so it needs to be outermost.
-}

module Monad
  ( module Monad
  , module Control.Monad.Except
  , module Control.Monad.State.Strict
  , module Control.Monad.Random
  ) where

import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Monad.Random
import Data.List (sort, sortOn)
import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH
import System.Random.Shuffle

import Character qualified as C
import Character (Character, CharacterType(..))
import Script (Script)

newtype R a = Randomizer { unR :: StateT RackState (ExceptT String IO) a }
  deriving
    ( Functor, Applicative, Monad
    , MonadError String, MonadState RackState, MonadRandom
    )

instance MonadFail R where
  fail :: String -> R a
  fail = throwError

type PlayerCount = Int
-- TODO: future type safety improvement: record promoted character type
-- in the type of the OTR?
data OneTypeRack = OTR
  { _reserve, _unprocessed, _unstable, _stable :: [Character] }

data RackState = RS
  { _townsfolk, _outsiders, _minions, _demons, _fabled :: OneTypeRack
  , _pretend :: [(Character, Character)]
  , _arbitraryOutsiders :: Bool
  , _outsiderCount :: Int
  , _playerCount :: Int
  , _current :: Maybe Character
  }

makeLenses ''OneTypeRack
makeLenses ''RackState

shuffle :: [a] -> R [a]
shuffle = shuffleM

choose :: [a] -> R a
choose options = do
  let len = length options
  n <- getRandomR (0, len-1)
  return $ options !! n

-- change the error message in an R.
withErr :: String -> R a -> R a
withErr msg r = r `catchError` \_ -> throwError msg

setCurrent :: Character -> R ()
setCurrent c = current .= Just c

currentDone :: R (Maybe Character)
currentDone = current <<.= Nothing

-- Given a list of all Characters available and a script of characters
-- to use, construct a RackState with all Characters in reserve,
-- no pretenders, unset arboutsiders, and no current processee.
initRackState :: [Character] -> [String] -> PlayerCount -> RackState
initRackState chars script pc =
  RS { _townsfolk = buildOTR Townsfolk 
     , _outsiders = buildOTR Outsider
     , _minions   = buildOTR Minion
     , _demons    = buildOTR Demon
     , _fabled    = buildOTR Fable
     , _pretend   = []
     , _arbitraryOutsiders = False
     , _outsiderCount = 0
     , _playerCount = pc
     , _current   = Nothing
     }
  where
    buildOTR ty = OTR [] [] [] [] & reserve .~ allOf ty
    chars' = sortOn C.id chars
    script' = sort script `extractedFrom` chars'
    extractedFrom :: [String] -> [Character] -> [Character]
    extractedFrom [] _     = []
      -- could just as well return [] in this case silently but... meh
    extractedFrom (n:_) [] = error $ "Unknown character: " ++ n
    extractedFrom (n:ns) (c:cs)
      | n == C.id c = c : extractedFrom ns cs
      | otherwise   = extractedFrom (n:ns) cs

    allOf :: CharacterType -> [Character]
    allOf ty = filter ((== ty) . C.ctype) script'

type Characters = [Character]
runR :: Characters -> Script -> PlayerCount -> R a -> IO (Either String a)
runR cs s pc r =
  runExceptT $ evalStateT (unR r) (initRackState cs s pc)
