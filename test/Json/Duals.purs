module Test.Polyform.Batteries.Json.Duals where

import Prelude
import Data.Argonaut (Json, fromBoolean, fromNumber) as Argonaut
import Data.Argonaut (fromArray, fromNumber, fromObject, fromString, jsonNull)
import Data.Argonaut (fromString) as Argounaut
import Data.Functor.Invariant (imap)
import Data.Generic.Rep (class Generic)
import Data.Int (toNumber)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Data.Validation.Semigroup (invalid, unV)
import Data.Variant (Variant, inj, match)
import Effect.Aff (Aff)
import Foreign.Object (fromFoldable) as Object
import Polyform.Batteries.Json (NullExpected, jnull)
import Polyform.Batteries.Json.Duals (CoproductErrors, IncorrectTag, _incorrectTag, arrayOf, boolean, int, noArgs, null, number, object, on, string, sum, (:=))
import Polyform.Batteries.Json.Duals (Dual, field, object, string) as Json.Duals
import Polyform.Batteries.Json.Validators (BooleanExpected, FieldMissing, IntExpected, NumberExpected, ObjectExpected, StringExpected, _stringExpected)
import Polyform.Batteries.Json.Validators (boolean, error, int, string) as Json.Validators
import Polyform.Dual (Dual(..)) as Dual
import Polyform.Dual (dual, parser, (~))
import Polyform.Dual.Record (build) as Dual.Record
import Polyform.Dual.Variant (case_)
import Polyform.Validator (liftFn) as Validator
import Polyform.Validator (liftFnMV) as Validtor
import Polyform.Validator (runValidator)
import Prelude (unit) as Prelude
import Test.Unit (TestSuite, failure, test)
import Test.Unit (suite) as Test.Unit
import Test.Unit.Assert (equal)
import Test.Utils (unsafeStringify)
import Type.Prelude (Proxy(..))
import Type.Row (type (+))

data Sum
  = S String
  | I Int
  | B Boolean
  | N Number
  | U Unit
  | E

derive instance genericSum ∷ Generic Sum _

derive instance eqSum ∷ Eq Sum

instance showSum ∷ Show Sum where
  show = genericShow

data Single
  = Single String

sumD' ∷
  Json.Duals.Dual
    Aff
    ( BooleanExpected
        + FieldMissing
        + IntExpected
        + IncorrectTag
        + NullExpected
        + NumberExpected
        + ObjectExpected
        + StringExpected
        + ()
    )
    Sum
sumD' =
  sum
    { "S": identity string
    , "I": identity int
    , "B": identity boolean
    , "N": identity number
    , "E": identity noArgs
    , "U": identity unitDual
    }

derive instance genericSingle ∷ Generic Single _

_b = Proxy ∷ Proxy "b"

_s = Proxy ∷ Proxy "s"

_u = Proxy ∷ Proxy "u"

_i = Proxy ∷ Proxy "i"

unitDual ∷ ∀ e m. Monad m ⇒ Json.Duals.Dual m (NullExpected + e) Unit
unitDual = imap (const unit) (const jnull) null

msg ∷ ∀ a. a → String
msg _ = ""

variant ∷
  forall e m.
  Monad m ⇒
  Json.Duals.Dual m (CoproductErrors + IntExpected + NullExpected + e) (Variant ( i ∷ Int, s ∷ String, u ∷ Unit ))
variant =
  case_
    # on (Proxy ∷ Proxy "s") string
    # on (Proxy ∷ Proxy "u") unitDual
    # on (Proxy ∷ Proxy "i") int

sumVariantDual ∷
  ∀ e.
  Json.Duals.Dual Aff
    ( BooleanExpected
        + FieldMissing
        + IncorrectTag
        + IntExpected
        + ObjectExpected
        + StringExpected
        + e
    )
    (Variant ( s ∷ String, b ∷ Boolean, i ∷ Int ))
sumVariantDual = Json.Duals.object >>> tagWithValue >>> valueDual
  where
  tagWithValue =
    Dual.Dual $ { t: _, v: _ }
      <$> _.t
      ~ Json.Duals.field "tag" Json.Duals.string
      <*> _.v
      ~ Json.Duals.field "value" identity

  parser =
    Validtor.liftFnMV
      $ case _ of
          { t: "s", v } → runValidator (Json.Validators.string >>> Validator.liftFn (inj _s)) v
          { t: "i", v } → runValidator (Json.Validators.int >>> Validator.liftFn (inj _i)) v
          { t: "b", v } → runValidator (Json.Validators.boolean >>> Validator.liftFn (inj _b)) v
          { t, v } → pure $ invalid $ Json.Validators.error _incorrectTag msg t

  serializer =
    match
      { s: \s → pure { t: "s", v: Argounaut.fromString s }
      , i: \i → pure { t: "i", v: Argonaut.fromNumber <<< toNumber $ i }
      , b: \b → pure { t: "b", v: Argonaut.fromBoolean $ b }
      }

  valueDual = dual parser serializer

suite :: TestSuite
suite =
  Test.Unit.suite "Test.Batteries.Json.Duals"
    $ do
        Test.Unit.suite "record handling"
          $ do
              let
                obj ∷
                  ∀ e.
                  Json.Duals.Dual Aff
                    ( FieldMissing
                        + IntExpected
                        + NumberExpected
                        + ObjectExpected
                        + StringExpected
                        + e
                    )
                    { foo ∷ Int, bar ∷ String, baz ∷ Number }
                obj = object >>> d
                  where
                  d =
                    Dual.Record.build
                      $ (Proxy ∷ Proxy "foo")
                      := int
                      <<< (Proxy ∷ Proxy "bar")
                      := string
                      <<< (Proxy ∷ Proxy "baz")
                      := number

                objs = arrayOf obj
              test "Parse object"
                $ do
                    let
                      input =
                        fromObject
                          $ Object.fromFoldable
                              [ "foo" /\ fromNumber (toNumber 8)
                              , "bar" /\ fromString "test"
                              , "baz" /\ fromNumber 8.0
                              ]

                      expected = { foo: 8, bar: "test", baz: 8.0 }
                    parsed ← runValidator (parser obj) input
                    -- void $ runSerializer objs []
                    unV
                      (const $ failure "Validation failed")
                      (_ `equal` expected)
                      parsed
        -- test "Tokenized"
        --   $ do
        --       let
        --         input =
        --           fromArray
        --             [ fromNumber (toNumber 8)
        --             , fromString "test"
        --             , fromNumber 8.0
        --             ]
        --         expected = { foo: 8, bar: "test", baz: 8.0 }
        --       parsed ← runValidator (parser obj) input
        --       -- void $ runSerializer objs []
        --       unV
        --         (const $ failure "Validation failed")
        --         (_ `equal` expected)
        --         parsed
        Test.Unit.suite "sum handling"
          $ do
              test "through generic helper"
                $ do
                    let
                      sumD ∷
                        Json.Duals.Dual
                          Aff
                          ( BooleanExpected
                              + FieldMissing
                              + IntExpected
                              + IncorrectTag
                              + NullExpected
                              + NumberExpected
                              + ObjectExpected
                              + StringExpected
                              + ()
                          )
                          Sum
                      sumD =
                        sum
                          { "S": (\a → a) string
                          , "I": (\a → a) int
                          , "B": (\a → a) boolean
                          , "N": (\a → a) number
                          , "E": (\a → a) noArgs
                          , "U": (\a → a) unitDual
                          }

                      s =
                        fromObject
                          $ Object.fromFoldable
                              [ "tag" /\ fromString "S", "value" /\ fromString "test" ]

                      e =
                        fromObject
                          $ Object.fromFoldable
                              [ "tag" /\ fromString "E", "value" /\ jsonNull ]

                      n =
                        fromObject
                          $ Object.fromFoldable
                              [ "tag" /\ fromString "N", "value" /\ fromNumber 8.0 ]

                      u =
                        fromObject
                          $ Object.fromFoldable
                              [ "tag" /\ fromString "U", "value" /\ jsonNull ]
                    parsedS ← runValidator (parser sumD) s
                    unV
                      (const $ failure "Validation failed")
                      (_ `equal` (S "test"))
                      parsedS
                    parsedE ← runValidator (parser sumD) e
                    unV
                      (const $ failure "Validation failed")
                      (_ `equal` E)
                      parsedE
                    parsedN ← runValidator (parser sumD) n
                    unV
                      (const $ failure "Validation failed")
                      (_ `equal` (N 8.0))
                      parsedN
                    parsedU ← runValidator (parser sumD) u
                    unV
                      (const $ failure "Validation failed")
                      (_ `equal` (U Prelude.unit))
                      parsedU
                    let
                      s' =
                        fromObject
                          $ Object.fromFoldable
                              [ "tag" /\ fromString "S", "value" /\ fromNumber 8.0 ]

                      _json = Proxy ∷ Proxy "json"

                      expectedError =
                        Json.Validators.error _stringExpected msg (Argonaut.fromNumber 8.0)
                          <> Json.Validators.error _incorrectTag msg "S"
                          <> Json.Validators.error _incorrectTag msg "S"
                          <> Json.Validators.error _incorrectTag msg "S"
                          <> Json.Validators.error _incorrectTag msg "S"
                          <> Json.Validators.error _incorrectTag msg "S"
                    parsedS' ← runValidator (parser sumD) s'
                    unV
                      ( \err →
                          when (err /= expectedError)
                            $ failure
                                ( "Expecting \""
                                    <> unsafeStringify expectedError
                                    <> "\" but got: \""
                                    <> unsafeStringify err
                                    <> "\""
                                )
                      )
                      (const $ failure "Expecting validation failure")
                      parsedS'
