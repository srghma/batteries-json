module Polyform.Batteries.Json.Duals where

import Prelude
import Data.Argonaut (Json, jsonNull)
import Data.Argonaut (fromArray, fromBoolean, fromNumber, fromObject, fromString) as Argonaut
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Either (note)
import Data.Enum (class BoundedEnum)
import Data.FoldableWithIndex (class FoldableWithIndex, foldlWithIndex)
import Data.Generic.Rep (class Generic, NoArguments)
import Data.Identity (Identity)
import Data.Int (toNumber) as Int
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Profunctor (lcmap)
import Data.Profunctor.Choice ((|||))
import Data.Profunctor.Star (Star(..))
import Data.Semigroup.First (First(..))
import Data.Traversable (traverse)
import Data.Validation.Semigroup (invalid)
import Data.Variant (Variant)
import Foreign.Object (Object) as Foreign
import Foreign.Object (empty, insert, singleton) as Object
import Polyform.Batteries (Dual) as Batteries
import Polyform.Batteries.Generic.Enum (InvalidEnumIndex)
import Polyform.Batteries.Generic.Enum (dual) as Enum
import Polyform.Batteries.Json.Validators (ArgonautError, ArrayExpected, BooleanExpected, Errors, FieldMissing, JNull, NullExpected, NumberExpected, ObjectExpected, StringExpected, IntExpected)
import Polyform.Batteries.Json.Validators (Errors, argonaut, array, arrayOf, boolean, error, field, fromNull, fromValidator, int, lmapValidatorVariant, null, nullable, number, object, objectOf, optionalField, string) as Json.Validators
import Polyform.Batteries.Json.Validators (mapOf) as Validators
import Polyform.Dual (Dual(..), DualD(..), hoistParser, parser) as Dual
import Polyform.Dual (dual, serializer, (~))
import Polyform.Dual.Generic.Sum (class GDualSum)
import Polyform.Dual.Generic.Sum (noArgs') as Dual.Generic.Sum
import Polyform.Dual.Generic.Variant (class GDualVariant)
import Polyform.Dual.Record (Builder, insert) as Dual.Record
import Polyform.Dual.Variant (on) as Dual.Variant
import Polyform.Type.Row (class Cons') as Row
import Polyform.Validator (Validator, liftFn, liftFnV) as Validator
import Polyform.Validator.Dual as Validator.Dual
import Polyform.Validator.Dual.Generic (sum, variant) as Validator.Dual.Generic
import Prim.Row (class Cons) as Row
import Prim.RowList (class RowToList)
import Type.Prelude (class IsSymbol, Proxy(..), Proxy(..), reflectSymbol)
import Type.Row (type (+))

type Base m errs
  = Validator.Dual.Dual m (Errors errs)

type Dual m errs
  = Base m errs Json

type Pure errs
  = Base Identity errs Json

type Field m errs
  = Base m errs (Object Json)

-- | Lift any Dual by enhancing error structure by adding an extra empty path.
-- | Please check `Duals.Validators.fromValidator` for the details.
fromDual ∷
  ∀ errs i m o.
  Monad m ⇒
  Batteries.Dual m errs i o →
  Base m errs i o
fromDual = Dual.hoistParser Json.Validators.fromValidator

lmapDualVariant ∷
  ∀ errs errs' m i o.
  Monad m ⇒
  (Variant errs → Variant errs') →
  Base m errs i o →
  Base m errs' i o
lmapDualVariant f = Dual.hoistParser (Json.Validators.lmapValidatorVariant f)

-- | We want to have Monoid for `Object` so we
-- | can compose serializations by monoidal
-- | `append`.
-- | Because `Foreign.Object` has monoid instance
-- | which performs underlining `append` on values
-- | we have to wrap it up and provide appropriate
-- | wrapping with `First`.
type Object a
  = Foreign.Object (First a)

-- | Raw object
objectOf ∷
  ∀ e o m.
  Monad m ⇒
  Dual m (ObjectExpected + e) o →
  Dual m (ObjectExpected + e) (Foreign.Object o)
objectOf (Dual.Dual (Dual.DualD prs ser)) = dual p s
  where
  p = Json.Validators.objectOf prs

  s = map Argonaut.fromObject <<< traverse ser

mapOf ∷
  ∀ e o m.
  Monad m ⇒
  Dual m (ObjectExpected + e) o →
  Dual m (ObjectExpected + e) (Map String o)
mapOf d = dual p s
  where
  p = Validators.mapOf (Dual.parser d)

  ser = serializer d

  s = map Argonaut.fromObject <<< traverse ser <<< objectFromFoldableWithIndex

  objectFromFoldableWithIndex :: forall f v. FoldableWithIndex String f => f v -> Foreign.Object v
  objectFromFoldableWithIndex = foldlWithIndex (\k m v -> Object.insert k v m) Object.empty

object ∷
  ∀ errs m.
  Monad m ⇒
  Dual m ( objectExpected ∷ Json | errs ) (Object Json)
object =
  dual
    (map First <$> Json.Validators.object)
    (pure <<< Argonaut.fromObject <<< map runFirst)
  where
  runFirst (First a) = a

-- | This `First` wrapper is necessary because `Object` provides
-- | `Semigroup` instance which based on value `Semigroup`.
-- | We use just left bias union here.
field ∷
  ∀ a e m.
  Monad m ⇒
  String →
  Dual m (FieldMissing + e) a →
  Field m (FieldMissing + e) a
field label d = dual prs ser
  where
  Dual.DualD fieldPrs fieldSer = un Dual.Dual d

  prs =
    lcmap
      (map (un First))
      (Json.Validators.field label fieldPrs)

  ser = map (Object.singleton label <<< First) <<< fieldSer

optionalField ∷
  ∀ a err m.
  Monad m ⇒
  String →
  Dual m err a →
  Field m err (Maybe a)
optionalField label d = dual prs ser
  where
  Dual.DualD fieldPrs fieldSer = un Dual.Dual d

  prs =
    lcmap
      (map (un First))
      (Json.Validators.optionalField label fieldPrs)

  vSer ∷ Maybe a → Identity Json
  vSer = un Star $ (Star pure ||| Star fieldSer) <<< Star (pure <<< note jsonNull)

  ser = pure <<< (Object.singleton label <<< First) <=< vSer

nullableOptionalField ∷
  ∀ a err m.
  Monad m ⇒
  String →
  Dual m (NullExpected + err) a →
  Field m (NullExpected + err) (Maybe a)
nullableOptionalField label d =
  dual (Validator.liftFn $ join) (pure <<< Just)
    <<< optionalField label (nullable d)

null ∷
  ∀ errs m.
  Monad m ⇒
  Dual m (NullExpected + errs) JNull
null =
  dual
    Json.Validators.null
    (pure <<< Json.Validators.fromNull)

nullable ∷
  ∀ a errs m.
  Monad m ⇒
  Dual m (NullExpected + errs) a →
  Dual m (NullExpected + errs) (Maybe a)
nullable d =
  dual
    (Json.Validators.nullable fieldPrs)
    ( case _ of
        Just a → fieldSer a
        Nothing → pure jsonNull
    )
  where
  Dual.DualD fieldPrs fieldSer = un Dual.Dual d

array ∷
  ∀ errs m.
  Monad m ⇒
  Dual m (ArrayExpected + errs) (Array Json)
array =
  dual
    Json.Validators.array
    (pure <<< Argonaut.fromArray)

arrayOf ∷
  ∀ e o m.
  Monad m ⇒
  Dual m (ArrayExpected + e) o →
  Dual m (ArrayExpected + e) (Array o)
arrayOf (Dual.Dual (Dual.DualD prs ser)) = dual (Json.Validators.arrayOf prs) (map Argonaut.fromArray <<< traverse ser)

int ∷
  ∀ errs m.
  Monad m ⇒
  Dual m (IntExpected + errs) Int
int =
  dual
    Json.Validators.int
    (pure <<< Argonaut.fromNumber <<< Int.toNumber)

boolean ∷
  ∀ errs m.
  Monad m ⇒
  Dual m (BooleanExpected + errs) Boolean
boolean =
  dual
    Json.Validators.boolean
    (pure <<< Argonaut.fromBoolean)

number ∷
  ∀ errs m.
  Monad m ⇒
  Dual m (NumberExpected + errs) Number
number =
  dual
    Json.Validators.number
    (pure <<< Argonaut.fromNumber)

string ∷
  ∀ errs m.
  Monad m ⇒
  Dual m (StringExpected + errs) String
string =
  dual
    Json.Validators.string
    (pure <<< Argonaut.fromString)

insert ∷
  ∀ e l o m prs prs' ser ser'.
  Row.Cons' l o ser ser' ⇒
  Row.Cons' l o prs prs' ⇒
  IsSymbol l ⇒
  Monad m ⇒
  Proxy l →
  Dual m (FieldMissing + e) o →
  Dual.Record.Builder
    (Validator.Validator m (Json.Validators.Errors (FieldMissing + e)))
    Identity
    (Object Json)
    { | ser' }
    { | prs }
    { | prs' }
insert label dual = Dual.Record.insert label (field (reflectSymbol label) dual)

infix 10 insert as :=

insertOptional ∷
  ∀ e m l o prs prs' ser ser'.
  Row.Cons' l (Maybe o) ser ser' ⇒
  Row.Cons' l (Maybe o) prs prs' ⇒
  IsSymbol l ⇒
  Monad m ⇒
  Proxy l →
  Dual m e o →
  Dual.Record.Builder
    (Validator.Validator m (Json.Validators.Errors e))
    Identity
    (Object Json)
    { | ser' }
    { | prs }
    { | prs' }
insertOptional label dual = Dual.Record.insert label (optionalField (reflectSymbol label) dual)

infix 10 insertOptional as :=?

insertConst ∷
  ∀ e l o m prs prs' ser ser'.
  Row.Cons' l o ser ser' ⇒
  Row.Cons' l o prs prs' ⇒
  IsSymbol l ⇒
  Monad m ⇒
  Proxy l →
  o →
  Dual.Record.Builder
    (Validator.Validator m (Json.Validators.Errors (FieldMissing + e)))
    Identity
    (Object Json)
    { | ser' }
    { | prs }
    { | prs' }
insertConst label a = label := (dual prs ser)
  where
  ser = const $ pure jsonNull

  prs = Validator.liftFn (const a)

type CoproductErrors e
  = (IncorrectTag + StringExpected + FieldMissing + ObjectExpected + e)

variant ∷
  ∀ e d dl m v.
  Monad m ⇒
  RowToList d dl ⇒
  GDualVariant (Validator.Validator m (Json.Validators.Errors (CoproductErrors + e))) Identity Json dl d v ⇒
  { | d } →
  Dual m (CoproductErrors + e) (Variant v)
variant = Validator.Dual.Generic.variant tagged

sum ∷
  ∀ a m e rep r.
  Monad m ⇒
  Generic a rep ⇒
  GDualSum (Validator.Validator m (Json.Validators.Errors (CoproductErrors + e))) Identity Json rep r ⇒
  { | r } →
  Dual m (CoproductErrors + e) a
sum r = Validator.Dual.Generic.sum tagged r

_incorrectTag = Proxy ∷ Proxy "incorrectTag"

type IncorrectTag e
  = ( incorrectTag ∷ String | e )

taggedWith ∷
  ∀ a e l m.
  Monad m ⇒
  IsSymbol l ⇒
  Proxy l →
  Base m (CoproductErrors + e) (Object Json) { t ∷ String, v ∷ Json } →
  Dual m (CoproductErrors + e) a →
  Dual m (CoproductErrors + e) a
taggedWith label tag (Dual.Dual (Dual.DualD prs ser)) = object >>> tag >>> tagged'
  where
  tagged' ∷ Base m (CoproductErrors + e) { t ∷ String, v ∷ Json } a
  tagged' =
    let
      fieldName = reflectSymbol label

      ser' = map { t: fieldName, v: _ } <<< ser

      msg t = "Incorrect tag: " <> show t

      prs' =
        prs
          <<< Validator.liftFnV \{ t, v } →
              if fieldName /= t then
                invalid $ Json.Validators.error _incorrectTag msg t
              else
                pure v
    in
      dual prs' ser'

defaultTagging ∷ ∀ e m. Monad m ⇒ Base m (CoproductErrors + e) (Object Json) { t ∷ String, v ∷ Json }
defaultTagging =
  Dual.Dual $ { t: _, v: _ }
    <$> _.t
    ~ field "tag" string
    <*> _.v
    ~ field "value" identity

tagged ∷
  ∀ a e l m.
  Monad m ⇒
  IsSymbol l ⇒
  Proxy l →
  Dual m (CoproductErrors + e) a →
  Dual m (CoproductErrors + e) a
tagged label = taggedWith label defaultTagging

on ∷
  ∀ a l e m r r'.
  Monad m ⇒
  Row.Cons l a r r' ⇒
  IsSymbol l ⇒
  Proxy l →
  Dual m (CoproductErrors + e) a →
  Dual m (CoproductErrors + e) (Variant r) →
  Dual m (CoproductErrors + e) (Variant r')
on label d rest = Dual.Variant.on tagged label d rest

infix 10 on as :>

noArgs ∷ ∀ e m. Monad m ⇒ Dual m e NoArguments
noArgs = Dual.Generic.Sum.noArgs' jsonNull

argonaut ∷ ∀ a e m. Monad m ⇒ EncodeJson a ⇒ DecodeJson a ⇒ Dual m (ArgonautError + e) a
argonaut = dual Json.Validators.argonaut ser
  where
  ser = (pure <<< encodeJson)

enum ∷ ∀ a e m. Monad m ⇒ BoundedEnum a ⇒ Proxy a → Dual m (IntExpected + InvalidEnumIndex + e) a
enum p = fromDual (Enum.dual p) <<< int

enum' ∷ ∀ a e m. Monad m ⇒ BoundedEnum a ⇒ Dual m (IntExpected + InvalidEnumIndex + e) a
enum' = fromDual (Enum.dual (Proxy ∷ Proxy a)) <<< int

-- enum' ∷ ∀ a e m. Monad m ⇒ BoundedEnum a ⇒ SingleField m (IntExpected + InvalidEnumIndex + e) a
-- enum' = Enum.dual' <<< Int.dual
-- decode ∷ ∀ a e. JsonDual Identity Identity e a → Json → Either (Validators.Errors (JsonError + e)) a
-- decode dual j =
--   unwrap $ unwrap (Validator.Dual.runValidator dual j)
-- 
-- encode ∷ ∀ a e. JsonDual Identity Identity e a → a → Json
-- encode dual = un Identity <<< Validator.Dual.runSerializer dual
-- 
