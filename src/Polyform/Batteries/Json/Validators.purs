module Polyform.Batteries.Json.Validators
  ( ArgonautError
  , ArrayExpected
  , Base
  , BooleanExpected
  , ErrorsBase
  , Errors
  , Field
  , FieldMissing
  , IntExpected
  , JNull
  , NullExpected
  , NumberExpected
  , ObjectExpected
  , Path
  , Pure
  , Segment(..)
  , StringExpected
  , Validator
  , _arrayExpected
  , _booleanExpected
  , _fieldMissing
  , _intExpected
  , _nullExpected
  , _numberExpected
  , _objectExpected
  , _stringExpected
  , argonaut
  , array
  , arrayOf
  , boolean
  , consErrorsPath
  , enum
  , enum'
  , error
  , fromValidator
  , liftErrors
  , lmapValidatorVariant
  , index
  , int
  , jnull
  , field
  , fromNull
  , mapOf
  , null
  , nullable
  , nullableOptionalField
  , number
  , object
  , objectOf
  , optionalField
  , printPath
  , string
  , toNull
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut (Json, stringify)
import Data.Argonaut (JsonDecodeError, fromArray, isNull, jsonNull, toArray, toBoolean, toNumber, toObject, toString) as Argonaut
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Array (fromFoldable, index, singleton) as Array
import Data.Bifunctor (lmap)
import Data.Either (note)
import Data.Enum (class BoundedEnum)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity)
import Data.Int (fromNumber) as Int
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map (fromFoldableWithIndex) as Map
import Data.Maybe (Maybe(..))
import Data.Profunctor (lcmap)
import Data.Profunctor.Choice ((|||))
import Data.Show.Generic (genericShow)
import Data.String (joinWith) as String
import Data.Traversable (sequence)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Validation.Semigroup (V)
import Data.Variant (Variant)
import Foreign.Object (Object)
import Foreign.Object (lookup) as Object
import Polyform.Batteries (Errors', Msg, Validator', error) as Batteries
import Polyform.Batteries.Generic.Enum (InvalidEnumIndex)
import Polyform.Batteries.Generic.Enum (validator) as Enum
import Polyform.Validator (Validator, liftFn, liftFnEither, liftFnMaybe, lmapValidator) as Validator
import Polyform.Validator (liftFn, liftFnMV, liftFnMaybe, runValidator)
import Prim.Row (class Cons) as Row
import Type.Prelude (class IsSymbol)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

-- | This error representation is for sure "object field / array element" biased.
-- | But it works fine in the most real live scenarios.
-- | For the top level values we just use `Nil` as a path (by using `liftErrors`).
-- |
-- | To simplify extensibility the error tree is flattened into
-- | list of error paths.
data Segment
  = Key String
  | Index Int

derive instance eqSegment ∷ Eq Segment

derive instance genericSegment ∷ Generic Segment _

instance showSegment ∷ Show Segment where
  show s = genericShow s

type Path = List Segment

printPath ∷ Path → String
printPath = String.joinWith "." <<< map printSegment <<< Array.fromFoldable
  where
  printSegment (Key k) = k

  printSegment (Index i) = show i

-- | TODO: turn errors into `NonEmptyArray`
type ErrorsBase errs = Array { path ∷ Path, errors ∷ Array errs }

type Errors errs = ErrorsBase (Batteries.Msg errs)

type Base m errs = Validator.Validator m (Errors errs)

type Field m errs = Base m errs (Object Json)

type Validator m errs = Base m errs Json

type Pure errs = Validator Identity errs

-- | TODO: Renamo to `liftValidator`
-- | Lifts validators which represents error as `Array (Variant errs)`
-- | into json validator by wrapping failures in empty path information.
-- |
-- | For example if you want to use some general purpose `String` validators
-- | in the context of json validation you can easily lift their error
-- | representation:
-- |
-- | ```
-- | nonBlankString ∷ ∀ e m. Monad m ⇒ Validator m (stringExpected ∷ Json, nonBlankExpected ∷ Unit | e) Json String
-- | nonBlankString = fromValidator String.nonBlank <<< string
-- | ```
fromValidator ∷ ∀ errs m i. Monad m ⇒ Batteries.Validator' m errs i ~> Base m errs i
fromValidator = Validator.lmapValidator liftErrors

error ∷ ∀ errs e l r. Row.Cons l e r errs ⇒ IsSymbol l ⇒ Proxy l → (e → String) → e → Errors errs
error label msg = liftErrors <<< Batteries.error label msg

liftErrors ∷ ∀ errs. Batteries.Errors' errs → Errors errs
liftErrors = Array.singleton <<< { path: Nil, errors: _ }

lmapValidatorVariant ∷ ∀ err err' i m o. Monad m ⇒ (Variant err → Variant err') → Base m err i o → Base m err' i o
lmapValidatorVariant f = Validator.lmapValidator (mapErrors f)

mapErrors ∷ ∀ err err'. (Variant err → Variant err') → Errors err → Errors err'
mapErrors f = map (\{ errors, path } → { path, errors: map step errors })
  where
  step { info, msg } = { info: f info, msg }

consErrorsPath ∷ ∀ e. Segment → Errors e → Errors e
consErrorsPath segment = map step
  where
  step { path, errors } = { path: segment : path, errors }

_objectExpected = Proxy ∷ Proxy "objectExpected"

type ObjectExpected e = (objectExpected ∷ Json | e)

object ∷ ∀ e m. Monad m ⇒ Validator m (ObjectExpected + e) (Object Json)
object = Validator.liftFnMaybe (error _objectExpected msg) Argonaut.toObject
  where
  msg = append "Object expected but got: " <<< stringify

objectOf
  ∷ ∀ m e a
  . Monad m
  ⇒ Validator m (ObjectExpected + e) a
  → Validator m (ObjectExpected + e) (Object a)
objectOf v = liftFnMV ov <<< object
  where
  ep key = consErrorsPath (Key key)

  -- | Run every validator by prefixing its error path with index.
  -- | Validator results in `m (V e a)` so we need traverse here.
  f ∷ Object Json → m (Object (V (Errors (ObjectExpected + e)) a))
  f = traverseWithIndex \idx → runValidator (Validator.lmapValidator (ep idx) v)

  ov ∷ Object Json → m (V (Errors (ObjectExpected + e)) (Object a))
  ov = f >>> map sequence

mapOf
  ∷ ∀ m e a
  . Monad m
  ⇒ Validator m (ObjectExpected + e) a
  → Validator m (ObjectExpected + e) (Map String a)
mapOf v = liftFn Map.fromFoldableWithIndex <<< objectOf v

field_
  ∷ ∀ a errs m
  . Monad m
  ⇒ String
  → Base m errs (Maybe Json) a
  → Field m errs a
field_ name fv = Validator.liftFn (Object.lookup name) >>> Validator.lmapValidator (consErrorsPath (Key name)) fv

_fieldMissing = Proxy ∷ Proxy "fieldMissing"

type FieldMissing e = (fieldMissing ∷ String | e)

-- | These two validators starts from `Object Json` and not just `Json` so
-- | you should compose them with `object` validator like:
-- |
-- | ```purescript
-- | object >>> ({ x: _, y: _ } <$> field "x" int <*> field "y" int)
-- | ```
-- |
-- | We don't provide this composition by default because these could
-- | lead easily to inefficient usage which would validate object multiple
-- | times like:
-- |
-- | { x: _, y: _ } <$> (object >>> field "x" int) <*> (object >>> field "y" int)
field
  ∷ ∀ a errs m
  . Monad m
  ⇒ String
  → Validator m (FieldMissing + errs) a
  → Field m (FieldMissing + errs) a
field name fv = field_ name (liftFnMaybe (const $ error _fieldMissing (const $ msg) name) identity >>> fv)
  where
  msg = "Object is missing a required field: " <> name

optionalField
  ∷ ∀ a errs m
  . Monad m
  ⇒ String
  → Validator m errs a
  → Field m errs (Maybe a)
optionalField name fv = field_ name v
  where
  v = lcmap (note unit) (pure Nothing ||| Just <$> fv)

nullableOptionalField
  ∷ ∀ a errs m
  . Monad m
  ⇒ String
  → Validator m (NullExpected + errs) a
  → Field m (NullExpected + errs) (Maybe a)
nullableOptionalField name fv = join <$> optionalField name (nullable fv)

_arrayExpected = Proxy ∷ Proxy "arrayExpected"

type ArrayExpected e = (arrayExpected ∷ Json | e)

array ∷ ∀ e m. Monad m ⇒ Validator m (ArrayExpected + e) (Array Json)
array = Validator.liftFnMaybe (error _arrayExpected msg) Argonaut.toArray
  where
  msg = append "Array expected but got: " <<< stringify

arrayOf
  ∷ ∀ m e a
  . Monad m
  ⇒ Validator m (ArrayExpected + e) a
  → Validator m (ArrayExpected + e) (Array a)
arrayOf v = array >>> liftFnMV av
  where
  ep idx = consErrorsPath (Index idx)

  -- | Run every validator by prefixing its error path with index.
  -- | Validator results in `m (V e a)` so we need traverse here.
  f ∷ Array Json → m (Array (V (Errors (ArrayExpected + e)) a))
  f = traverseWithIndex \idx → runValidator (Validator.lmapValidator (ep idx) v)

  av ∷ Array Json → m (V (Errors (ArrayExpected + e)) (Array a))
  av = f >>> map sequence

_indexMissing = Proxy ∷ Proxy "indexMissing"

type IndexMissing e = (indexMissing ∷ { arr ∷ Array Json, idx ∷ Int } | e)

index
  ∷ ∀ errs m
  . Monad m
  ⇒ Int
  → Validator.Validator m (Errors (IndexMissing + errs)) (Array Json) Json
index idx = liftFnMaybe err (flip Array.index idx)
  where
  err arr = consErrorsPath (Index idx) (error _indexMissing msg { arr, idx })

  msg e =
    "Given array array: "
      <> stringify (Argonaut.fromArray e.arr)
      <> " is missing an element at index : "
      <> show e.idx

_nullExpected = Proxy ∷ Proxy "nullExpected"

type NullExpected e = (nullExpected ∷ Json | e)

null ∷ ∀ e m. Monad m ⇒ Validator m (NullExpected + e) JNull
null = Validator.liftFnMaybe (error _nullExpected msg) toNull
  where
  msg = append "Null expected but got: " <<< stringify

nullable
  ∷ ∀ a errs m
  . Monad m
  ⇒ Validator m (NullExpected + errs) a
  → Validator m (NullExpected + errs) (Maybe a)
nullable fv = (null *> pure Nothing) <|> (Just <$> fv)

_intExpected = Proxy ∷ Proxy "intExpected"

type IntExpected e = (intExpected ∷ Json | e)

int ∷ ∀ e m. Monad m ⇒ Validator m (IntExpected + e) Int
int = Validator.liftFnMaybe (error _intExpected msg) (Argonaut.toNumber >=> Int.fromNumber)
  where
  msg = append "Int expected but got: " <<< stringify

_booleanExpected = Proxy ∷ Proxy "booleanExpected"

type BooleanExpected e = (booleanExpected ∷ Json | e)

boolean ∷ ∀ e m. Monad m ⇒ Validator m (BooleanExpected + e) Boolean
boolean = Validator.liftFnMaybe (error _booleanExpected msg) Argonaut.toBoolean
  where
  msg = append "Boolean expected but got: " <<< stringify

_stringExpected = Proxy ∷ Proxy "stringExpected"

type StringExpected e = (stringExpected ∷ Json | e)

string ∷ ∀ e m. Monad m ⇒ Validator m (StringExpected + e) String
string = Validator.liftFnMaybe (error _stringExpected msg) Argonaut.toString
  where
  msg :: Json -> String
  msg = append "String expected but got: " <<< stringify

_numberExpected = Proxy ∷ Proxy "numberExpected"

type NumberExpected e = (numberExpected ∷ Json | e)

number ∷ ∀ e m. Monad m ⇒ Validator m (NumberExpected + e) Number
number = Validator.liftFnMaybe (error _numberExpected msg) Argonaut.toNumber
  where
  msg = append "Number expected but got: " <<< stringify

-- | Because argonaut is not providing this type for us any more we define
-- | it here so we can provide a result from `null` validator.
-- | value as `jsonNull`.
foreign import data JNull ∷ Type

instance eqJNull ∷ Eq JNull where
  eq _ _ = true

instance ordJNull ∷ Ord JNull where
  compare _ _ = EQ

jnull ∷ JNull
jnull = unsafeCoerce Argonaut.jsonNull

toNull ∷ Json → Maybe JNull
toNull = Argonaut.isNull >>> if _ then Just jnull else Nothing

fromNull ∷ JNull → Json
fromNull = unsafeCoerce

_argonautError = Proxy ∷ Proxy "argonautError"

type ArgonautError e = (argonautError ∷ Argonaut.JsonDecodeError | e)

argonaut ∷ ∀ a e m. Monad m ⇒ DecodeJson a ⇒ Validator m (ArgonautError + e) a
argonaut = Validator.liftFnEither (lmap (error _argonautError msg) <<< decodeJson)
  where
  msg _ = "Argonaut decoding error."

enum ∷ ∀ a err m. Monad m ⇒ BoundedEnum a ⇒ Proxy a → Validator m (IntExpected + InvalidEnumIndex + err) a
enum p = fromValidator (Enum.validator p) <<< int

enum' ∷ ∀ a err m. Monad m ⇒ BoundedEnum a ⇒ Validator m (IntExpected + InvalidEnumIndex + err) a
enum' = fromValidator (Enum.validator (Proxy ∷ Proxy a)) <<< int
