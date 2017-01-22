{-# LANGUAGE OverloadedStrings #-}

module Talos.Sql.Encoder ( queryToSQL
                         , findToSQL
                         ) where

import Data.Foldable

import qualified Language.SQL.SimpleSQL.Syntax as S
import qualified Data.Bson as B
import qualified Data.Text as T

import Talos.Types

findToSQL :: FindExpr -> Either String S.QueryExpr
findToSQL (FindExpr col query proj) = do
  qExpr <- queryToSQL query
  slist <- projToSL proj
  return $ S.makeSelect { S.qeSelectList    = slist
                        , S.qeFrom          = [toTableRef col]
                        , S.qeWhere         = Just qExpr
                        }
  where
    toTableRef txt = S.TRSimple [S.Name $ T.unpack txt]
    projToSL :: Projection -> Either String [(S.ValueExpr, Maybe S.Name)]
    projToSL (Projection pl) = traverse f pl
    f (ProjInclude field) = Right (S.Iden [S.Name $ T.unpack field], Nothing)
    -- TODO : move to error monad to report this is not supported
    f (ProjExclude field) = Left "Exclude projection constraints are not supported"

queryToSQL :: QueryExpr -> Either String S.ValueExpr

queryToSQL (ExprConstr op) = opToSQL op
queryToSQL (ExprOR exprs) = S.Parens <$> foldExprs (S.Name "OR") exprs
queryToSQL (ExprAND exprs) = S.Parens <$> foldExprs (S.Name "AND") exprs
queryToSQL (ExprNOT expr) = S.Parens <$> (S.PrefixOp [S.Name "NOT"] <$> queryToSQL expr)
queryToSQL expr = Left ("No SQL equivilent for expression " ++ show expr)

foldExprs :: S.Name -> [QueryExpr] -> Either String S.ValueExpr
foldExprs name [] = Left ("Cannot fold empty expression list for " ++ show name)
foldExprs _ [expr] = queryToSQL expr
foldExprs name (expr:exprs) = S.BinOp <$> queryToSQL expr
                                      <*> pure [name] <*> foldExprs name exprs

opToSQL :: QueryOp -> Either String S.ValueExpr
opToSQL (OpEQ field value) = mkBinOp field "=" value
opToSQL (OpLT field value) = mkBinOp field "<" value
opToSQL (OpGT field value) = mkBinOp field ">" value
opToSQL (OpGE field value) = mkBinOp field ">=" value
opToSQL (OpLE field value) = mkBinOp field "<=" value
opToSQL (OpIN field values) = S.In True fieldId <$> inList where
  fieldId = S.Iden $ mkNameList field
  parsed = traverse valToExpr values
  inList = S.InList <$> parsed

opToSQL (OpMOD field div rem) = Right $ S.BinOp (fieldToSQL field) (mkOp "=") modExpr where
  modExpr = S.BinOp (S.NumLit $ show div) (mkOp "%") (S.NumLit $ show rem)

opToSQL (OpEXISTS field bool) = if bool then Right subexpr
                                else Right $ S.PrefixOp [S.Name "NOT"] subexpr
  where
    subexpr = S.PrefixOp [S.Name "EXISTS"] $ S.Iden $ mkNameList field

opToSQL expr = Left $ "Unsupported expression " ++ show expr
-- TODO : All these operators
{-
opToSQL (OpREGEX field pat opts) = undefined
opToSQL (OpTEXT field txt opts) = undefined
opToSQL (OpEMATCH field exprs) = undefined
opToSQL (OpSIZE field size) = undefined
opToSQL (OpTYPE field btype) = undefined
-}

-- |
-- = Utility functions

mkBinOp :: Field -> T.Text -> DocValue -> Either String S.ValueExpr
mkBinOp field op value = S.BinOp (fieldToSQL field) (mkOp op) <$> valToExpr value

mkOp :: T.Text -> [S.Name]
mkOp opStr = [S.Name $ T.unpack opStr]

mkNameList :: T.Text -> [S.Name]
mkNameList name = S.Name . T.unpack <$> T.splitOn "." name


valToExpr :: DocValue -> Either String S.ValueExpr
valToExpr (B.Float x) = Right $ S.NumLit (show x)
valToExpr (B.String txt) = Right $ S.StringLit (T.unpack txt)
valToExpr (B.Bool b) = Right $ S.Iden (mkNameList v) where
  v = if b then "TRUE" else "FALSE"
valToExpr B.Null = Right $ S.Iden (mkNameList "NULL")
valToExpr (B.Int32 x) = Right $ S.NumLit (show x)
valToExpr (B.Int64 x) = Right $ S.NumLit (show x)
-- TODO : Proper implementation of documents as SQL values
valToExpr val = Right $ S.App (mkNameList "bson") [S.StringLit (show val)]

valToName :: DocValue -> Either String [S.Name]
valToName (B.String "") = Left "Names cannot be empty strings"
valToName (B.String txt) = Right $ (S.Name . T.unpack) <$> T.splitOn "." txt
valToName val = Left $ "Unrecognised DocValue: " ++ show val

fieldToSQL :: Field -> S.ValueExpr
fieldToSQL field = S.Iden [S.Name . T.unpack $ field]
