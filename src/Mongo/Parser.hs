{-# LANGUAGE OverloadedStrings #-}

module Mongo.Parser (
  query
  ) where

import qualified Data.Aeson as A
import Data.Aeson.Types (Parser, parse)
import Data.Aeson ((.:), (.=))
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V
import Data.Scientific as S
import Control.Applicative

import Types

instance A.FromJSON Expr where
  parseJSON = query

query :: A.Value -> Parser Expr
query (A.Object o) = do
  exprs <- (mapM kvpair (H.toList o))
  return $ case exprs of
    -- single value case
    [expr] -> expr
    -- multiple value case
    lst -> ExprAND lst

-- | parse a property/value pair of a MongoDB JSON expression
kvpair :: (Field, A.Value) -> Parser Expr
kvpair (k, v) = logic k v
            <|> constraints k v
            <|> pure (ExprEQ k v)

logic :: Field -> A.Value -> Parser Expr
logic "$not" expr = ExprNOT <$> query expr
logic op expr = A.withArray "Logical operator" f expr where
  f a = case op of
    "$or" -> ExprOR <$> mapM query (V.toList a)
    "$and" -> ExprAND <$> mapM query (V.toList a)
    "$nor" -> ExprAND <$> mapM query (V.toList a)

constraints :: Field -> A.Value -> Parser Expr
constraints k = A.withObject "Object as constraints" $ \o -> do
  vals <- H.foldlWithKey' fAcc (pure []) o
  return $ case vals of
    -- One constraint only
    [x] -> x
    -- Multiple constraints
    xs -> ExprAND xs
  where
    fAcc acc op v  = liftA2 (:) (unaryOp k op v) acc

unaryOp :: Field -> T.Text  -> A.Value -> Parser Expr
unaryOp field "$eq" v            = ExprEQ field <$> pure v
unaryOp field "$lt" v            = ExprLT field <$> pure v
unaryOp field "$gt" v            = ExprGT field <$> pure v
unaryOp field "$lte" v           = ExprLE field <$> pure v
unaryOp field "$gte" v           = ExprGE field <$> pure v
unaryOp field "$in" (A.Array a)  = ExprIN field <$> pure a
unaryOp field "$nin" (A.Array a) = ExprNIN field <$> pure a
unaryOp field "$all" (A.Array a) = ExprALL field <$> pure a
unaryOp field "$exists" (A.Bool b) = ExprEXISTS field <$> pure b
unaryOp field "$type" (A.Number b) = ExprTYPE field <$> case (toBoundedInteger b) of
    Nothing -> empty
    Just i  -> pure i
unaryOp field "$size" (A.Number b) = ExprSIZE field <$> case (toBoundedInteger b) of
    Nothing -> empty
    Just i  -> pure i
unaryOp field "$elemMatch" (A.Array a) = ExprEMATCH field <$> mapM query (V.toList a)

{- TODO
  | ExprREGEX Field T.Text (Maybe T.Text)
  | ExprTEXT Field T.Text (Maybe T.Text)
-}
