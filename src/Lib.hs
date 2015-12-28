{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( query
    , Expr(..)
    ) where

import Language.SQL.SimpleSQL.Parser
import Language.SQL.SimpleSQL.Syntax
import Language.SQL.SimpleSQL.Pretty
import qualified Data.Aeson as A
import Data.Aeson.Types (Parser, parse)
import Data.Aeson ((.:), (.=))
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H
import Data.Scientific as S
import Control.Applicative

someFunc :: IO ()
someFunc = putStrLn.show $ parseQueryExpr SQL2011 "" Nothing "select a + 2"


-- Convenience function for parsing expressions
parseExpression :: String -> Either ParseError ValueExpr
parseExpression = parseValueExpr SQL2011 "main" (Just (0, 0))

prettyExpression :: ValueExpr -> String
prettyExpression = prettyValueExpr SQL2011

type Field = T.Text
type Query = [Expr]

{-
A query {field1: op1, field2: op2, ...} is effectively
  field1 op1 AND field2 op2 AND ...
or field* can be a logical operator

Therefore the first level of parsing is to AND together key/value pairs
-}

data Expr =
    ExprEQ Field A.Value
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
  | ExprEMATCH Field Query
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

-- TODO : instance FromJSON Expr where

query :: A.Value -> Parser Expr
query (A.Object o) = ExprAND <$> (mapM kvpair (H.toList o))

-- | parse a property/value pair of a MongoDB JSON expression
kvpair :: (Field, A.Value) -> Parser Expr
kvpair (k, v) = logic k v
            <|> constraints k v
            <|> pure (ExprEQ k v)


{-
  | ExprOR [Expr]
  | ExprAND [Expr]
  | ExprNOT Expr
  | ExprNOR [Expr]
-}
logic :: Field -> A.Value -> Parser Expr
logic k v = empty

constraints :: Field -> A.Value -> Parser Expr
constraints k (A.Object o) = do
  vals <- H.foldlWithKey' fAcc (pure []) o
  return $ case vals of
    -- One constraint only
    [x] -> x
    -- Multiple constraints
    xs -> ExprAND xs
  where
    fAcc acc op v  = liftA2 (:) (unaryOp k op v) acc
constraints _ _ = empty

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
-- TODO : $elemMatch requires parser context
-- unaryOp field "$elemMatch" (A.Array a) = ExprEMATCH $ query a

{-
  | ExprREGEX Field T.Text (Maybe T.Text)
  | ExprTEXT Field T.Text (Maybe T.Text)
-}



-- ---------------------------------------------------------------------------
-- Properties
