{-# LANGUAGE OverloadedStrings #-}
{-
Loading Scripts.
-}

module Script (Script, loadScript) where

import Data.Aeson
import Data.Text (unpack)
import Data.Aeson.Types (prependFailure, unexpected)

type Script = [String]

newtype ScriptCharacter =
  ScriptCharacter { name :: String }

instance FromJSON ScriptCharacter where
  parseJSON (Object v) = ScriptCharacter <$> v .: "id"
  parseJSON (String s) = pure $ ScriptCharacter $ unpack s
  parseJSON invalid =
    prependFailure "parsing ScriptCharacter failed, " (unexpected invalid)

loadScript :: FilePath -> IO Script
loadScript fp = do
  m <- decodeFileStrict @[ScriptCharacter] fp
  case m of
    Nothing -> fail "Unable to parse script"
    Just scs -> pure $ removeMeta $ map name scs
  where
    removeMeta = filter (/= "_meta")
