module Polyform.Batteries.Json.Tokenized.Duals where

import Prelude

import Data.Argonaut (Json)
import Data.Identity (Identity)
import Polyform.Batteries.Json.Duals (Dual) as Json.Duals
import Polyform.Batteries.Json.Tokenized.Validators (EndExpected, _endExpected)
import Polyform.Batteries.Json.Validators (Errors) as Json.Validators
import Polyform.Batteries.Json.Validators (FieldMissing, _fieldMissing, error)
import Polyform.Tokenized.Validator.Dual (Dual, end, liftUntokenized) as Tokenized.Validator.Dual
import Type.Row (type (+))

type Dual m errs
  = Tokenized.Validator.Dual.Dual m (Json.Validators.Errors errs) Json

type Pure errs
  = Dual Identity errs

-- | TODO: Error reporting is crappy at the moment.
-- | We should probably carry on the index on the
-- | base `Tokenized` level.
item ∷
  ∀ a errs m.
  Monad m ⇒
  Json.Duals.Dual m (FieldMissing + errs) a →
  Dual m (FieldMissing + errs) a
item = Tokenized.Validator.Dual.liftUntokenized (error _fieldMissing msg idx)
  where
  msg _ = "Unexpected end of input"

  idx = "<tokenized index>"

end ∷
  ∀ errs m.
  Monad m ⇒
  Dual m (EndExpected + errs) Unit
end = Tokenized.Validator.Dual.end (error _endExpected msg)
  where
  msg _ = "Expecting an end of input"
