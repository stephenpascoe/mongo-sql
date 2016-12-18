{-# LANGUAGE OverloadedStrings #-}
-- TODO : change constraint terminology into Op.  Move field name into Op constructors
module Mongo.Parser () where

import Control.Applicative
import Data.Maybe
import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V
import qualified Data.Scientific as S
import Types

instance FromJSON Expr where
  parseJSON = query

{-
A field dict is a dict of field names where values are field constraints
-}
query :: Value -> Parser Expr
query (Object obj) = collapseLogic . ExprAND <$> traverse f (H.toList obj) where
  f ("$and", Array a) = ExprAND <$> traverse query (V.toList a)
  f ("$or", Array a) = ExprOR <$> traverse query (V.toList a)
  f (field, val) = fieldConstraint field val
query _ = empty

-- Remove duplicate and/or/not
collapseLogic :: Expr -> Expr
collapseLogic (ExprAND [expr]) = collapseLogic expr
collapseLogic (ExprAND exprs) = ExprAND $ map collapseLogic exprs
collapseLogic (ExprOR [expr])  = collapseLogic expr
collapseLogic (ExprOR exprs) = ExprOR $ map collapseLogic exprs
collapseLogic (ExprNOT expr) = ExprNOT $ collapseLogic expr
collapseLogic expr = expr

{-
A fieldConstraint is either a single value or a constraintExpr
-}
fieldConstraint :: Field -> Value -> Parser Expr
fieldConstraint field (Object h) = ExprAND <$> traverse f (H.toList h) where
  f ("$nin", val) = ExprNOT . ExprConstr <$> constraint "$in" field val
  f ("$all", Array a) = ExprAND <$> traverse (fieldConstraint field) (V.toList a)
  f (op, val) = ExprConstr <$> constraint op field val

fieldConstraint field val = ExprConstr <$> constraint "$eq" field val

{-
constraint
-}
constraint :: Operator  -> Field -> Value -> Parser FieldOp
constraint "$eq" f v            = pure $ eq f v
constraint "$lt" f v            = pure $ lt f v
constraint "$gt" f v            = pure $ gt f v
constraint "$lte" f v           = pure $ lte f v
constraint "$gte" f v           = pure $ gte f v
constraint "$in" f (Array a)    = pure $ in_ f (V.toList a)
constraint "$exists" f (Bool b) = pure $ exists f b
constraint "$type" f (Number x) = type_ f <$> case S.floatingOrInteger x of
    Left _ -> empty
    Right i  -> pure $ intToBsonType i
constraint "$size" f (Number x) = size f <$> case S.floatingOrInteger x of
    Left _ -> empty
    Right i  -> pure i
constraint "$elemMatch" f (Array a) = ematch f <$> mapM query (V.toList a)

constraint "$mod" field (Array a) = maybe empty pure $ do
  [divisor, remainder] <- sequenceA $ map f (V.toList a)
  return $ mod_ field divisor remainder
  where
    f (Number x) = S.toBoundedInteger x


constraint op field _ = fail $ "Constraint " ++ T.unpack op ++ " on " ++
                               T.unpack field ++ " not regognised"



{- TODO
  | ExprREGEX Field T.Text (Maybe T.Text)
  | ExprTEXT Field T.Text (Maybe T.Text)
-}
