module Lib
    ( parseExpression
    , queryExprToSQL
    , jsonToSQL
    , findToSqlText
    ) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Language.SQL.SimpleSQL.Parser as SP
import qualified Language.SQL.SimpleSQL.Syntax as SS
import qualified Language.SQL.SimpleSQL.Pretty as SY
import qualified Data.Aeson as A

import Types
import Mongo.Parser
import Sql.Encoder

-- Convenience function for parsing expressions
parseExpression :: String -> Either SP.ParseError SS.ValueExpr
parseExpression = SP.parseValueExpr SS.SQL2011 "main" (Just (0, 0))

prettySqlExpr :: SS.ValueExpr -> T.Text
prettySqlExpr = T.pack . SY.prettyValueExpr SS.SQL2011

prettySqlQuery :: SS.QueryExpr -> T.Text
prettySqlQuery = T.pack . SY.prettyQueryExpr SS.SQL2011

queryExprToSQL :: QueryExpr -> Maybe T.Text
queryExprToSQL expr = prettySqlExpr <$> queryToSQL expr

jsonToSQL :: BL.ByteString -> Maybe T.Text
jsonToSQL jsonTxt = do
  json <- A.decode jsonTxt
  queryExprToSQL json

findToSqlText :: FindExpr -> Maybe T.Text
findToSqlText expr = prettySqlQuery <$> findToSQL expr
