module Polyform.Batteries.Json.Tokenized.Validators where

import Prelude
import Data.Argonaut (Json)
import Polyform.Batteries.Json.Validators (Errors, Validator) as Json.Validators
import Polyform.Batteries.Json.Validators (FieldMissing, _fieldMissing, error)
import Polyform.Tokenized.Validator (Validator, end) as Tokenized.Validator
import Polyform.Tokenized.Validator (liftUntokenized)
import Type.Prelude (Proxy(..))
import Type.Row (type (+))

type Validator m errs = Tokenized.Validator.Validator m (Json.Validators.Errors errs) Json

-- | TODO: Error reporting is crappy at the moment.
-- | We should probably carry on the index on the
-- | base `Tokenized` level.
item
  ∷ ∀ a errs m
  . Monad m
  ⇒ Json.Validators.Validator m (FieldMissing + errs) a
  → Validator m (FieldMissing + errs) a
item = liftUntokenized (error _fieldMissing (const $ "Unexpected end of input") "<tokenized index>")

_endExpected = Proxy ∷ Proxy "endExpected"

type EndExpected e = (endExpected ∷ Json | e)

end
  ∷ ∀ errs m
  . Monad m
  ⇒ Validator m (EndExpected + errs) Unit
end = Tokenized.Validator.end (error _endExpected msg)
  where
  msg _ = "Expecting an end of input"
