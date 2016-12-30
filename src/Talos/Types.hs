{-# LANGUAGE OverloadedStrings #-}

module Talos.Types (
    QueryExpr (..)
  , QueryOp (..)
  , Field
  , DocValue
  , Operator
  , BsonType
  , FindExpr (..)
  , Projection (..)
  , ProjOp (..)
  , intToBsonType
  , bsonTypeToInt
  ) where

import qualified Data.Text as T
import qualified Data.Aeson as A
import qualified Data.Bson as B


type Field = T.Text
type Operator = T.Text
type DocValue = B.Value
type Collection = T.Text


data FindExpr = FindExpr Collection QueryExpr Projection deriving (Show, Eq)

{-
A query {field1: op1, field2: op2, ...} is effectively
  field1 op1 AND field2 op2 AND ...
or field* can be a logical operator

Therefore the first level of parsing is to AND together key/value pairs
-}

data QueryExpr = ExprConstr QueryOp
               | ExprOR [QueryExpr]
               | ExprAND [QueryExpr]
               | ExprNOT QueryExpr
                 -- Note: $nor expands to ExprNOT (ExprOR A B)
               deriving (Show, Eq)

data QueryOp = OpEQ Field DocValue
             | OpLT Field DocValue
             | OpGT Field DocValue
             | OpGE Field DocValue
             | OpLE Field DocValue
               -- $ne expands to ExprNOT (ExprConstr (OpEQ field value))
             | OpIN Field [DocValue]
               -- $nin expands to ExprNot (ExprConstr (OpIN field array))
             | OpMOD Field Int Int
             | OpREGEX Field T.Text (Maybe T.Text)
             | OpTEXT Field T.Text (Maybe T.Text)
               -- $all expands to ExprAnd [(ExprConstr (OpEQ field val1)),
               --                          (ExprConstr (OpEQ field val2))]
             | OpEMATCH Field [QueryExpr]
             | OpSIZE Field Integer
             | OpEXISTS Field Bool
             | OpTYPE Field BsonType
             deriving (Show, Eq)

data Projection = Projection [ProjOp]
                  deriving (Show, Eq)
data ProjOp = ProjInclude Field
            | ProjExclude Field
              -- TODO : Projection operators
              deriving (Show, Eq)

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

bsonTypeToInt :: BsonType -> Integer
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


intToBsonType :: Integer -> BsonType
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
