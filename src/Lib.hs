{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Language.SQL.SimpleSQL.Parser
import Language.SQL.SimpleSQL.Syntax
import Language.SQL.SimpleSQL.Pretty
import qualified Data.Aeson as A
import Data.Aeson.Types (Parser, parse)
-- import Data.Attoparsec.ByteString (Parser)
import Data.Aeson ((.:), (.=))
import qualified Data.Text as T
import Data.HashMap.Strict as H
import Data.Scientific as S

import Test.QuickCheck

someFunc :: IO ()
someFunc = putStrLn.show $ parseQueryExpr SQL2011 "" Nothing "select a + 2"


-- Convenience function for parsing expressions
parseExpression :: String -> Either ParseError ValueExpr
parseExpression = parseValueExpr SQL2011 "main" (Just (0, 0))

prettyExpression :: ValueExpr -> String
prettyExpression = prettyValueExpr SQL2011

type Field = T.Text
type Query = [Expr]

data Expr =
    ExprEQ Field A.Value
  | ExprLT Field S.Scientific
  | ExprLE Field S.Scientific
  | ExprGT Field S.Scientific
  | ExprGE Field S.Scientific
  | ExprNE Field S.Scientific
  | ExprIN Field [A.Value]
  | ExprNIN Field [A.Value]
  | ExprOR [Expr]
  | ExprAND [Expr]
  | ExprNOT Expr
  | ExprNOR [Expr]
  | ExprMOD Field Int Int
  | ExprREGEX Field T.Text (Maybe T.Text)
  | ExprTEXT Field T.Text (Maybe T.Text)
  | ExprWHERE T.Text
  | ExprALL Field [A.Value]
  | ExprEMATCH Field Query
  | ExprSIZE Field Int
  | ExprEXISTS Field Bool
  | ExprTYPE Field Int
  deriving (Show, Eq)

-- TODO: Geospatial operators
-- TODO: Projection operators

-- TODO : instance FromJSON Expr where


-- Let's try and parse a simple expression {"foo": {"$eq": "bar"}}
-- NOTE: {a: {$eq: b} == {a: b}

parseEq :: A.Value -> Parser Expr
parseEq (A.Object o) = return $ ExprAND (H.foldlWithKey' acc [] o)
  where
    acc as k v = build k v : as
    build k v = ExprEQ k v

prop_parseEq :: String -> String -> Bool
prop_parseEq a b = parse parseEq (A.object [ a' .= b' ]) == A.Success (ExprAND [ExprEQ a' (A.String b')]) where
  a' = T.pack a
  b' = T.pack a

prop_parseEq2 :: String -> String -> Bool
prop_parseEq2 a b =
  parse parseEq (A.object [ a' .= A.object [ "$eq" .= b' ] ]) == A.Success (ExprAND [ExprEQ a' (A.String b')]) where
    a' = T.pack a
    b' = T.pack a

check = mapM quickCheck [prop_parseEq, prop_parseEq2]
