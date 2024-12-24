{-
Dump Monad state, using the IO layer.
-}

module Monad.Dump (dumpState) where

import Control.Monad.State.Class

import Character qualified as C
import Monad

dumpState :: R ()
dumpState = do
  RS tf o m d f p ao oc pc c <- get
  dumpOTR "townsfolk" tf
  dumpOTR "outsiders" o
  dumpOTR "minions" m
  dumpOTR "demons" d
  dumpOTR "fabled" f
  dumpPretend p
  dumpMsg $
    "ArbOutsiders: " ++ show ao
    ++ ", OutsiderCount: " ++ show oc
    ++ ", PlayerCount: " ++ show pc
    ++ ", Current: " ++ maybe "none" C.id c

dumpMsg :: String -> R ()
dumpMsg s = Randomizer (liftIO $ putStrLn s)

dumpPretend :: [(C.Character, C.Character)] -> R ()
dumpPretend ps = do
  dumpMsg "Pretenders:"
  forM_ ps $ \(pretender, replacement) ->
    dumpMsg $ "  " ++ C.id pretender ++ " replaced by " ++ C.id replacement

dumpOTR :: String -> OneTypeRack -> R ()
dumpOTR name OTR{_reserve,_unprocessed,_unstable,_stable} = do
  dumpMsg $ name ++ ":"
  forM_ sets $ \(sname, members) -> do
    dumpMsg $ "  " ++ sname ++ ":"
    dumpMsg $ "    " ++ show (map C.id members)
  where
    sets =
      [ ("reserve", _reserve)
      , ("unprocessed", _unprocessed)
      , ("unstable", _unstable)
      , ("stable", _stable)
      ]
