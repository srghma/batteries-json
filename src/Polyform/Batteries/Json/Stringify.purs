module Polyform.Batteries.Json.Stringify where

import Prelude
import Data.Argonaut (Json)
import Data.Foldable (foldMap)
import Data.Identity (Identity)
import Data.Lazy (force) as Lazy
import Foreign.Object (Object)
import Polyform (Validator) as Polyform
import Polyform.Batteries (Msg) as Batteries
import Polyform.Batteries.Json.Duals (Base) as Json.Duals
import Polyform.Batteries.Json.Validators (Base) as Json.Validators
import Polyform.Batteries.Json.Validators (Path)
import Polyform.Batteries.Stringify (Errors) as Batteries.Stringify
import Polyform.Validator (lmapValidator)
import Polyform.Validator.Dual (Dual) as Polyform.Validator.Dual
import Polyform.Validator.Dual (lmapDual)

type Error
  = { path ∷ Path, errors ∷ Array String }

type Errors
  = Array Error

type ValidatorBase m
  = Polyform.Validator m Errors

type Field m o
  = ValidatorBase m (Object Json) o

type Validator m o
  = ValidatorBase m Json o

type Pure o
  = Validator Identity o

stringifyError ∷ ∀ errs. { path ∷ Path, errors ∷ Array (Batteries.Msg errs) } → Error
stringifyError { path, errors } = { path, errors: map (Lazy.force <<< _.msg) errors }

stringifyValidator ∷ ∀ m errs i o. Monad m ⇒ Json.Validators.Base m errs i o → ValidatorBase m i o
stringifyValidator = lmapValidator (map stringifyError)

type DualBase m
  = Polyform.Validator.Dual.Dual m Errors

stringifyDual ∷ ∀ m errs i o. Monad m ⇒ Json.Duals.Base m errs i o → DualBase m i o
stringifyDual = lmapDual (map stringifyError)

flatten ∷ Errors → Batteries.Stringify.Errors
flatten = foldMap step
  where
  step { path, errors } = map (append (show path <> ": ")) errors
