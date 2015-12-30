{-# LANGUAGE OverloadedStrings #-}

module Types (
    Expr (..)
  , Field
  ) where

import qualified Data.Text as T
import qualified Data.Aeson as A


type Field = T.Text

{-
A query {field1: op1, field2: op2, ...} is effectively
  field1 op1 AND field2 op2 AND ...
or field* can be a logical operator

Therefore the first level of parsing is to AND together key/value pairs
-}

data Expr = ExprEQ Field A.Value
          | ExprLT Field A.Value
          | ExprLE Field A.Value
          | ExprGT Field A.Value
          | ExprGE Field A.Value
          | ExprNE Field A.Value
          | ExprIN Field A.Array
          | ExprNIN Field A.Array
          | ExprMOD Field Int Int
          | ExprREGEX Field T.Text (Maybe T.Text)
          | ExprTEXT Field T.Text (Maybe T.Text)
          | ExprALL Field A.Array
          | ExprEMATCH Field [Expr]
          | ExprSIZE Field Int
          | ExprEXISTS Field Bool
          | ExprTYPE Field Int
          | ExprOR [Expr]
          | ExprAND [Expr]
          | ExprNOT Expr
          | ExprNOR [Expr]
          deriving (Show, Eq)

-- TODO: Geospatial operators
-- TODO: Projection operators
