{-# LANGUAGE OverloadedStrings #-}

module Talos.Sql.Encoder ( queryToSQL
                         , findToSQL
                         ) where

import Data.Foldable

import qualified Language.SQL.SimpleSQL.Syntax as S
import qualified Data.Bson as B
import qualified Data.Text as T

import Talos.Types

findToSQL :: FindExpr -> Maybe S.QueryExpr
findToSQL (FindExpr col query proj) = do
  qExpr <- queryToSQL query
  slist <- projToSL proj
  return $ S.makeSelect { S.qeSelectList    = slist
                        , S.qeFrom          = [toTableRef col]
                        , S.qeWhere         = Just qExpr
                        }
  where
    toTableRef txt = S.TRSimple [S.Name $ T.unpack txt]
    projToSL :: Projection -> Maybe [(S.ValueExpr, Maybe S.Name)]
    projToSL (Projection pl) = traverse f pl
    f (ProjInclude field) = Just (S.Iden [S.Name $ T.unpack field], Nothing)
    -- TODO : move to error monad to report this is not supported
    f (ProjExclude field) = Nothing

queryToSQL :: QueryExpr -> Maybe S.ValueExpr

queryToSQL (ExprConstr op) = opToSQL op
queryToSQL (ExprOR exprs) = foldExprs (S.Name "OR") exprs
queryToSQL (ExprAND exprs) = foldExprs (S.Name "AND") exprs
queryToSQL (ExprNOT expr) = S.PrefixOp [S.Name "NOT"] <$> queryToSQL expr

foldExprs :: S.Name -> [QueryExpr] -> Maybe S.ValueExpr
foldExprs name [] = Nothing
foldExprs _ [expr] = queryToSQL expr
foldExprs name (expr:exprs) = S.BinOp <$> queryToSQL expr <*> pure [name] <*> foldExprs name exprs

opToSQL :: QueryOp -> Maybe S.ValueExpr
opToSQL (OpEQ field value) = mkBinOp field "=" value
opToSQL (OpLT field value) = mkBinOp field "<" value
opToSQL (OpGT field value) = mkBinOp field ">" value
opToSQL (OpGE field value) = mkBinOp field ">=" value
opToSQL (OpLE field value) = mkBinOp field "<=" value
opToSQL (OpIN field values) = S.In True fieldId <$> inList where
  fieldId = S.Iden $ mkNameList field
  parsed = traverse valToExpr values
  inList = S.InList <$> parsed

opToSQL (OpMOD field div rem) = Just $ S.BinOp (fieldToSQL field) (mkOp "=") modExpr where
  modExpr = S.BinOp (S.NumLit $ show div) (mkOp "%") (S.NumLit $ show rem)

opToSQL (OpEXISTS field bool) = if bool then Just subexpr
                                else Just $ S.PrefixOp [S.Name "NOT"] subexpr
  where
    subexpr = S.PrefixOp [S.Name "EXISTS"] $ S.Iden $ mkNameList field

-- TODO : All these operators
opToSQL (OpREGEX field pat opts) = undefined
opToSQL (OpTEXT field txt opts) = undefined
opToSQL (OpEMATCH field exprs) = undefined
opToSQL (OpSIZE field size) = undefined
opToSQL (OpTYPE field btype) = undefined


-- |
-- = Utility functions

mkBinOp :: Field -> T.Text -> DocValue -> Maybe S.ValueExpr
mkBinOp field op value = S.BinOp (fieldToSQL field) (mkOp op) <$> valToExpr value

mkOp :: T.Text -> [S.Name]
mkOp opStr = [S.Name $ T.unpack opStr]

mkNameList :: T.Text -> [S.Name]
mkNameList name = S.Name . T.unpack <$> T.splitOn "." name

-- TODO : Proper implementation of documents as SQL values
valToExpr :: DocValue -> Maybe S.ValueExpr
valToExpr (B.String txt) = Just $ S.StringLit (T.unpack txt)
valToExpr val = Just $ S.App (mkNameList "bson") [S.StringLit (show val)]


valToName :: DocValue -> Maybe [S.Name]
valToName (B.String "") = Nothing
valToName (B.String txt) = Just $ (S.Name . T.unpack) <$> T.splitOn "." txt
valToName _ = Nothing

fieldToSQL :: Field -> S.ValueExpr
fieldToSQL field = S.Iden [S.Name . T.unpack $ field]
