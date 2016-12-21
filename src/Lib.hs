module Lib
    ( parseExpression
    , queryExprToSQL
    ) where

import qualified Data.Text as T
import qualified Language.SQL.SimpleSQL.Parser as SP
import qualified Language.SQL.SimpleSQL.Syntax as SS
import qualified Language.SQL.SimpleSQL.Pretty as SY

import Types
import Mongo.Parser
import Sql.Encoder

-- Convenience function for parsing expressions
parseExpression :: String -> Either SP.ParseError SS.ValueExpr
parseExpression = SP.parseValueExpr SS.SQL2011 "main" (Just (0, 0))

prettySqlExpr :: SS.ValueExpr -> T.Text
prettySqlExpr = T.pack . SY.prettyValueExpr SS.SQL2011


queryExprToSQL :: QueryExpr -> Maybe T.Text
queryExprToSQL expr = prettySqlExpr <$> queryToSQL expr
