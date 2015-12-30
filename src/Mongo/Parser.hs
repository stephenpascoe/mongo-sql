{-# LANGUAGE OverloadedStrings #-}

module Mongo.Parser () where

import Control.Applicative

import Data.Aeson
import Data.Aeson.Types (Parser, parse)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V
import qualified Data.Scientific as S
import Types

instance FromJSON Expr where
  parseJSON = query

query :: Value -> Parser Expr
query (Object o) = do
  exprs <- (mapM kvpair (H.toList o))
  return $ case exprs of
    -- single value case
    [expr] -> expr
    -- multiple value case
    lst -> ExprAND lst

-- | parse a property/value pair of a MongoDB JSON expression
kvpair :: (Field, Value) -> Parser Expr
kvpair (k, v) = logic k v
            <|> constraints k v
            <|> pure (ExprEQ k v)

logic :: Field -> Value -> Parser Expr
logic "$not" expr = ExprNOT <$> query expr
logic op expr = withArray "Logical operator" f expr where
  f a = case op of
    "$or" -> ExprOR <$> mapM query (V.toList a)
    "$and" -> ExprAND <$> mapM query (V.toList a)
    "$nor" -> ExprAND <$> mapM query (V.toList a)

constraints :: Field -> Value -> Parser Expr
constraints k = withObject "Object as constraints" $ \o -> do
  vals <- H.foldlWithKey' fAcc (pure []) o
  return $ case vals of
    -- One constraint only
    [x] -> x
    -- Multiple constraints
    xs -> ExprAND xs
  where
    fAcc acc op v  = liftA2 (:) (unaryOp k op v) acc

unaryOp :: Field -> T.Text  -> Value -> Parser Expr
unaryOp field "$eq" v            = ExprEQ field <$> pure v
unaryOp field "$lt" v            = ExprLT field <$> pure v
unaryOp field "$gt" v            = ExprGT field <$> pure v
unaryOp field "$lte" v           = ExprLE field <$> pure v
unaryOp field "$gte" v           = ExprGE field <$> pure v
unaryOp field "$in" (Array a)  = ExprIN field <$> pure a
unaryOp field "$nin" (Array a) = ExprNIN field <$> pure a
unaryOp field "$all" (Array a) = ExprALL field <$> pure a
unaryOp field "$exists" (Bool b) = ExprEXISTS field <$> pure b
unaryOp field "$type" (Number x) = ExprTYPE field <$> case (S.toBoundedInteger x) of
    Nothing -> empty
    Just i  -> pure i
unaryOp field "$size" (Number x) = ExprSIZE field <$> case (S.toBoundedInteger x) of
    Nothing -> empty
    Just i  -> pure i
unaryOp field "$elemMatch" (Array a) = ExprEMATCH field <$> mapM query (V.toList a)

{- TODO
  | ExprMOD Field Int Int
  | ExprREGEX Field T.Text (Maybe T.Text)
  | ExprTEXT Field T.Text (Maybe T.Text)
-}
