{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( query
    , Expr(..)
    ) where

import Language.SQL.SimpleSQL.Parser
import Language.SQL.SimpleSQL.Syntax
import Language.SQL.SimpleSQL.Pretty

import Types
import Mongo.Parser

someFunc :: IO ()
someFunc = putStrLn.show $ parseQueryExpr SQL2011 "" Nothing "select a + 2"


-- Convenience function for parsing expressions
parseExpression :: String -> Either ParseError ValueExpr
parseExpression = parseValueExpr SQL2011 "main" (Just (0, 0))

prettyExpression :: ValueExpr -> String
prettyExpression = prettyValueExpr SQL2011
