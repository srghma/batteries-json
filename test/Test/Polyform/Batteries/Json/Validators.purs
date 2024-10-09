module Test.Polyform.Batteries.Json.Validators where

import Prelude

import Data.Argonaut (Json, fromArray, fromNumber, fromObject, fromString)
import Data.Int (toNumber)
import Data.Lazy (defer)
import Data.List (fromFoldable) as List
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Variant (inj)
import Effect.Aff (Aff)
import Foreign.Object (fromFoldable) as Object
import Polyform.Batteries.Json.Validators (Segment(..), _fieldMissing, _intExpected, array, consErrorsPath, error, field, int, liftErrors, number, object, string)
import Polyform.Batteries.Json.Validators (Validator) as Json
import Polyform.Tokenized (unliftUntokenized) as Tokenized
import Polyform.Tokenized.Validator (liftUntokenized) as Tokenized.Validator
import Polyform.Validator (liftFn) as Validator
import Polyform.Validator (runValidator)
import Record.Extra (sequenceRecord)
import Test.Polyform.Batteries.Json.Messages (mkFieldMissingMsg, mkIntExpectedMsg)
import Test.Polyform.Batteries.Json.Duals (unV)
import Test.Unit (TestSuite, failure, test)
import Test.Unit (suite) as Test.Unit
import Test.Unit.Assert (equal)
import Test.Utils (unsafeStringify)

suite ∷ TestSuite
suite =
  Test.Unit.suite "Test.Polyform.Json.Validators"
    $ do
        test "Tokenized"
          $ do
              let
                arrayToList = Validator.liftFn List.fromFoldable
                obj = array >>> arrayToList >>> d
                  where
                  fm = error _fieldMissing mkFieldMissingMsg

                  d =
                    Tokenized.unliftUntokenized
                      $ { foo: _, bar: _, baz: _ }
                      <$> Tokenized.Validator.liftUntokenized (fm "X") int
                      <*> Tokenized.Validator.liftUntokenized (fm "X") string
                      <*> Tokenized.Validator.liftUntokenized (fm "X") number

                input =
                  fromArray
                    [ fromNumber (toNumber 8)
                    , fromString "test"
                    , fromNumber 8.0
                    ]

                expected = { foo: 8, bar: "test", baz: 8.0 }
              parsed ← runValidator obj input
              unV
                (const $ failure "Validation failed")
                (_ `equal` expected)
                parsed
        test "Parse object"
          $ do
              let
                obj = object >>> d
                  where
                  d =
                    sequenceRecord
                      { foo: field "foo" int
                      , bar: field "bar" string
                      , baz: field "baz" number
                      }

                input =
                  fromObject
                    $ Object.fromFoldable
                        [ "foo" /\ fromNumber (toNumber 8)
                        , "bar" /\ fromString "test"
                        , "baz" /\ fromNumber 8.0
                        ]

                expected = { foo: 8, bar: "test", baz: 8.0 }
              parsed ← runValidator obj input
              unV
                (const $ failure "Validation failed")
                (_ `equal` expected)
                parsed
        test "Errors paths"
          $ do
              let
                obj ∷
                  Json.Validator
                    Aff
                    ( fieldMissing :: String
                    , intExpected :: Json
                    , numberExpected :: Json
                    , objectExpected :: Json
                    , stringExpected :: Json
                    )
                    _
                obj = object >>> r
                  where
                  r =
                    sequenceRecord
                      { foo: field "foo" sub
                      , bar: field "bar" string
                      , baz: field "baz" number
                      }

                  sub =
                    object
                      >>> sequenceRecord
                          { x: field "x" int
                          , y: field "y" int
                          }

                input =
                  fromObject
                    $ Object.fromFoldable
                        [ Tuple "foo" $ fromObject $ Object.fromFoldable [ "x" /\ fromString "incorrect int" ]
                        , "bar" /\ fromString "test"
                        , "baz" /\ fromNumber 8.0
                        ]
                expectedError =
                  consErrorsPath (Key "foo")
                    $ do
                        let v = fromString "incorrect int"
                        consErrorsPath (Key "x") (liftErrors [ { msg: defer \_ → mkIntExpectedMsg v, info: inj _intExpected v } ])
                    <> consErrorsPath (Key "y") (liftErrors [ { msg: defer \_ → mkFieldMissingMsg "y", info: inj _fieldMissing "y" } ])
              parsed ← runValidator obj input
              unV
                ( \err →
                    when (err /= expectedError)
                      $ failure
                          ( "Expecting:\n\""
                              <> unsafeStringify expectedError
                              <> "\"\n    but got:\n\""
                              <> unsafeStringify err
                              <> "\""
                          )
                )
                (const $ failure ("Validation should fail"))
                parsed
