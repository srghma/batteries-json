module Test.Main where

import Prelude
import Effect (Effect)
import Test.Polyform.Batteries.Json.Duals (suite) as Test.Polyform.Batteries.Json.Duals
import Test.Polyform.Batteries.Json.Validators (suite) as Test.Polyform.Batteries.Json.Validators
import Test.Unit.Main (runTest)

main ∷ Effect Unit
main =
  runTest
    $ do
        Test.Polyform.Batteries.Json.Validators.suite
        Test.Polyform.Batteries.Json.Duals.suite
