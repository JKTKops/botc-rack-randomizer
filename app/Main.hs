module Main (main) where

import Control.Monad
import System.Environment
import System.Exit (exitFailure)

import Randomizer (rack)
import Script (loadScript)

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 2) $ printUsage >> exitFailure
  let [path, pcs] = args
      pc = read pcs
  script <- loadScript path
  rack script pc

printUsage :: IO ()
printUsage = putStrLn $ unlines
  [ "Usage: rack-botc SCRIPTFILE PC"
  , "  SCRIPTFILE\t\tthe path to a suitable .json file for the script"
  , "  PC        \t\tthe playercount to rack for"
  ]
