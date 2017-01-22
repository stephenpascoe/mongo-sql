module Talos.Lib
    ( parseExpression
    , queryExprToSQL
    , jsonToSQL
    , findToSqlText
    , FindExpr(..)
    ) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Language.SQL.SimpleSQL.Parser as SP
import qualified Language.SQL.SimpleSQL.Syntax as SS
import qualified Language.SQL.SimpleSQL.Pretty as SY
import qualified Data.Aeson as A

import Talos.Types
import Talos.Mongo.Parser
import Talos.Sql.Encoder

-- Convenience function for parsing expressions
parseExpression :: String -> Either SP.ParseError SS.ValueExpr
parseExpression = SP.parseValueExpr SS.SQL2011 "main" (Just (0, 0))

prettySqlExpr :: SS.ValueExpr -> T.Text
prettySqlExpr = T.pack . SY.prettyValueExpr SS.SQL2011

prettySqlQuery :: SS.QueryExpr -> T.Text
prettySqlQuery = T.pack . SY.prettyQueryExpr SS.SQL2011

queryExprToSQL :: QueryExpr -> Either String T.Text
queryExprToSQL expr = prettySqlExpr <$> queryToSQL expr

jsonToSQL :: BL.ByteString -> Either String T.Text
jsonToSQL jsonTxt = do
  json <- A.eitherDecode jsonTxt
  queryExprToSQL json

findToSqlText :: FindExpr -> Either String T.Text
findToSqlText expr = prettySqlQuery <$> findToSQL expr
