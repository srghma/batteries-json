module Test.Utils where

import Prelude
import Data.Argonaut (Json) as Argonaut
import Data.Argonaut (stringify)
import Unsafe.Coerce (unsafeCoerce)

unsafeStringify :: forall t9. t9 -> String
unsafeStringify = stringify <<< unsafeJson
  where
  unsafeJson ∷ ∀ a. a → Argonaut.Json
  unsafeJson = unsafeCoerce
