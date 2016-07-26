{-# LANGUAGE OverloadedStrings #-}

module Types (
    Expr (..)
  , FieldConstraint (..)
  , Field
  , BsonType
  , intToBsonType
  , bsonTypeToInt
  ) where

import qualified Data.Text as T
import qualified Data.Aeson as A


type Field = T.Text
type Value = A.Value

{-
A query {field1: op1, field2: op2, ...} is effectively
  field1 op1 AND field2 op2 AND ...
or field* can be a logical operator

Therefore the first level of parsing is to AND together key/value pairs
-}

data FieldConstraint = ConstrEQ Value
                    | ConstrLT Value
                    | ConstrLE Value
                    | ConstrGT Value
                    | ConstrGE Value
                    -- $ne expands to ExprNOT (ExprConstr field (ConstrEq value))
                    | ConstrIN [Value]
                    -- $nin expands to ExprNot (ExprConstr field (ConstrIn array))
                    | ConstrMOD Int Int
                    | ConstrREGEX T.Text (Maybe T.Text)
                    | ConstrTEXT T.Text (Maybe T.Text)
                    -- $all expands to ExprAnd (ExprConstr field val1) (ExprConstr field val2)
                    | ConstrEMATCH [Expr]
                    | ConstrSIZE Int
                    | ConstrEXISTS Bool
                    | ConstrTYPE BsonType
                    deriving (Show, Eq)

data Expr = ExprConstr Field FieldConstraint
          | ExprOR Expr Expr
          | ExprAND Expr Expr
          | ExprNOT Expr
          -- Note: $nor expands to ExprNOT (ExprOR A B)
          deriving (Show, Eq)

-- TODO: use bson library?
data BsonType = Double            -- 1
              | String            -- 2
              | Object            -- 3
              | Array             -- 4
              | Binary            -- 5
              | Undefined         -- 6
              | ObjectId          -- 7
              | Boolean           -- 8
              | Date              -- 9
              | Null              -- 10
              | Regex             -- 11
              | DBPointer         -- 12
              | JavaScript        -- 13
              | Symbol            -- 14
              | JavaScriptScoped  -- 15
              | Integer32         -- 16
              | Timestamp         -- 17
              | Integer64         -- 18
              | MinKey            -- -1
              | MaxKey            -- 127
              deriving (Show, Eq)

bsonTypeToInt :: BsonType -> Int
bsonTypeToInt Double = 1
bsonTypeToInt String = 2
bsonTypeToInt Object = 3
bsonTypeToInt Array = 4
bsonTypeToInt Binary = 5
bsonTypeToInt Undefined = 6
bsonTypeToInt ObjectId = 7
bsonTypeToInt Boolean = 8
bsonTypeToInt Date = 9
bsonTypeToInt Null = 10
bsonTypeToInt Regex = 11
bsonTypeToInt DBPointer = 12
bsonTypeToInt JavaScript = 13
bsonTypeToInt Symbol = 14
bsonTypeToInt JavaScriptScoped = 15
bsonTypeToInt Integer32 = 16
bsonTypeToInt Timestamp = 17
bsonTypeToInt Integer64 = 18
bsonTypeToInt MinKey = -1
bsonTypeToInt MaxKey = 127


intToBsonType :: Int -> BsonType
intToBsonType  1 = Double
intToBsonType  2 = String
intToBsonType  3 = Object
intToBsonType  4 = Array
intToBsonType  5 = Binary
intToBsonType  6 = Undefined
intToBsonType  7 = ObjectId
intToBsonType  8 = Boolean
intToBsonType  9 = Date
intToBsonType  10 = Null
intToBsonType  11 = Regex
intToBsonType  12 = DBPointer
intToBsonType  13 = JavaScript
intToBsonType  14 = Symbol
intToBsonType  15 = JavaScriptScoped
intToBsonType  16 = Integer32
intToBsonType  17 = Timestamp
intToBsonType  18 = Integer64
intToBsonType  (-1) = MinKey
intToBsonType  127 = MaxKey

-- TODO: Geospatial operators
-- TODO: Projection operators
