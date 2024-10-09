module Test.Polyform.Batteries.Json.Messages where

import Prelude

import Data.Argonaut as Argonaut

-- These helpers mimic the oring string msg construction
-- because we don't want to expose them as a part of public API.
mkStringMsg :: Argonaut.Json -> String
mkStringMsg = append "String expected but got: " <<< Argonaut.stringify

mkIncorrectTagMsg :: forall a. Show a => a -> String
mkIncorrectTagMsg = append "Incorrect tag: " <<< show

mkFieldMissingMsg :: String -> String
mkFieldMissingMsg name = "Object is missing a required field: " <> name

mkIntExpectedMsg :: Argonaut.Json -> String
mkIntExpectedMsg = append "Int expected but got: " <<< Argonaut.stringify
