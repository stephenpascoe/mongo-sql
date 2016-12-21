{-# LANGUAGE OverloadedStrings #-}

module Mongo.Parser () where

import Control.Applicative
import Data.Maybe
import Data.Aeson as A
import Data.Aeson.Types (Parser, typeMismatch)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V
import qualified Data.Scientific as S
import qualified Data.Bson as B

import Types

instance FromJSON QueryExpr where
  parseJSON = query

{-
A field dict is a dict of field names where values are field constraints
-}
query :: A.Value -> Parser QueryExpr
query (Object obj) = collapseLogic . ExprAND <$> traverse f (H.toList obj) where
  f ("$and", Array a) = ExprAND <$> traverse query (V.toList a)
  f ("$or", Array a) = ExprOR <$> traverse query (V.toList a)
  f (field, val) = fieldConstraint field val
query _ = empty

-- Remove duplicate and/or/not
collapseLogic :: QueryExpr -> QueryExpr
collapseLogic (ExprAND [expr]) = collapseLogic expr
collapseLogic (ExprAND exprs) = ExprAND $ fmap collapseLogic exprs
collapseLogic (ExprOR [expr])  = collapseLogic expr
collapseLogic (ExprOR exprs) = ExprOR $ fmap collapseLogic exprs
collapseLogic (ExprNOT expr) = ExprNOT $ collapseLogic expr
collapseLogic expr = expr

{-
A fieldConstraint is either a single value or a constraintExpr
-}
fieldConstraint :: Field -> A.Value -> Parser QueryExpr
fieldConstraint field (Object h) = ExprAND <$> traverse f (H.toList h) where
  f ("$nin", val) = ExprNOT . ExprConstr <$> constraint "$in" field val
  f ("$all", Array a) = ExprAND <$> traverse (fieldConstraint field) (V.toList a)
  f ("$ne", val) = ExprNOT . ExprConstr <$> (OpEQ field <$> bsonifyValue val)
  f (op, val) = ExprConstr <$> constraint op field val

fieldConstraint field val = ExprConstr <$> constraint "$eq" field val

{-
constraint
-}
constraint :: Operator  -> Field -> A.Value -> Parser QueryOp
constraint "$eq" f v            = OpEQ f <$> bsonifyValue v
constraint "$lt" f v            = OpLT f <$> bsonifyValue v
constraint "$gt" f v            = OpGT f <$> bsonifyValue v
constraint "$lte" f v           = OpLE f <$> bsonifyValue v
constraint "$gte" f v           = OpGE f <$> bsonifyValue v
constraint "$in" f (Array a)    = OpIN f <$> traverse bsonifyValue (V.toList a)
constraint "$exists" f (Bool b) = pure $ OpEXISTS f b
constraint "$type" f (Number x) = OpTYPE f <$> case S.floatingOrInteger x of
    Left _ -> empty
    Right i  -> pure $ intToBsonType i
constraint "$size" f (Number x) = OpSIZE f <$> case S.floatingOrInteger x of
    Left _ -> empty
    Right i  -> pure i
constraint "$elemMatch" f (Array a) = OpEMATCH f <$> traverse query (V.toList a)

constraint "$mod" field (Array a) = maybe empty pure $ do
  [divisor, remainder] <- traverse f (V.toList a)
  return $ OpMOD field divisor remainder
  where
    f (Number x) = S.toBoundedInteger x


constraint op field _ = fail $ "Constraint " ++ T.unpack op ++ " on " ++
                               T.unpack field ++ " not regognised"



{- TODO
  | ExprREGEX Field T.Text (Maybe T.Text)
  | ExprTEXT Field T.Text (Maybe T.Text)
-}

toBson :: A.Object -> Parser B.Document
toBson = traverse f . H.toList where
    f (t, v) = (B.:=) t <$> bsonifyValue v

bsonifyValue :: A.Value -> Parser B.Value
bsonifyValue (Object obj) = B.Doc <$> toBson obj
bsonifyValue (A.Array array) = B.Array <$> traverse bsonifyValue (V.toList array)
bsonifyValue (A.String str) = pure $ B.String str
bsonifyValue (Number n) = case S.floatingOrInteger n of
  Left r  -> pure $ B.val (r :: Double)
  Right i -> pure $ B.val (i :: Integer)

bsonifyValue (A.Bool b) = pure $ B.Bool b
bsonifyValue A.Null = pure B.Null
