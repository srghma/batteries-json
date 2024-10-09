module Polyform.Batteries.Json.Messages where

-- | TODO: Provide a way to flatten represntation of json errors (with paths)
-- | Provide a helper which flattens any errors using "unsafeStringify" or something.

import Prelude

import Data.Argonaut (Json, fromObject, fromString)
import Foreign.Object (fromHomogeneous)
import Polyform.Batteries.Json.Validators (ArrayExpected, BooleanExpected, FieldMissing, IntExpected, NullExpected, NumberExpected, ObjectExpected, StringExpected) as Json
import Prim.Row (class Nub) as Row
import Prim.RowList (class RowToList)
import Record.Builder (Builder, merge) as Record.Builder
import Type.Row (type (+))
import Type.Row.Homogeneous (class HomogeneousRowList)

type FieldMessages msgs
  =
  ( Json.ArrayExpected
      + Json.BooleanExpected
      + Json.FieldMissing
      + Json.IntExpected
      + Json.NullExpected
      + Json.NumberExpected
      + Json.ObjectExpected
      + Json.StringExpected
      + msgs
  )

type FieldPrinters r
  =
  ( "arrayExpected" ∷ Json → Json
  , "booleanExpected" ∷ Json → Json
  , "fieldMissing" ∷ Json → Json
  , "intExpected" ∷ Json → Json
  , "nullExpected" ∷ Json → Json
  , "numberExpected" ∷ Json → Json
  , "objectExpected" ∷ Json → Json
  , "stringExpected" ∷ Json → Json
  | r
  )

field
  ∷ ∀ r r''
  . Row.Nub (FieldPrinters r) r''
  ⇒ Record.Builder.Builder { | r } { | r'' }
field =
  let
    flatten ∷ ∀ s sl. RowToList s sl ⇒ HomogeneousRowList sl Json ⇒ Record s → Json
    flatten = fromObject <<< fromHomogeneous

    printers ∷ { | FieldPrinters () }
    printers =
      { arrayExpected: \i → flatten { err: fromString "arrayExpected", value: i }
      , booleanExpected: \i → flatten { err: fromString "booleanExpected", value: i }
      , fieldMissing: \i → flatten { err: fromString "fieldMissing", value: i }
      , intExpected: \i → flatten { err: fromString "intExpected", value: i }
      , nullExpected: \i → flatten { err: fromString "nullExpected", value: i }
      , numberExpected: \i → flatten { err: fromString "numberExpected", value: i }
      , objectExpected: \i → flatten { err: fromString "objectExpected", value: i }
      , stringExpected: \i → flatten { err: fromString "stringExpected", value: i }
      }
  in
    Record.Builder.merge printers
