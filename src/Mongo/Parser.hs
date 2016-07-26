module Mongo.Parser () where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V
import qualified Data.Scientific as S
import Types

instance FromJSON Expr where
  parseJSON = query

-- TODO : Use higher-order functions here
query :: Value -> Parser Expr
query (Object o) = query' (H.toList o) where
  query' [] = empty
  query' [(field, expr)] = fieldExpression field expr
  query' ((field, expr) : kvs) = ExprAND <$> fieldExpression field expr
                                         <*> query' kvs
query _ = empty


-- | parse a property/value pair of a MongoDB JSON expression
fieldExpression :: Field -> Value -> Parser Expr
fieldExpression k v =  fieldLogic k v
                   <|> fieldConstraints k v
                   <|> pure (ExprConstr k (ConstrEQ v))

fieldLogic :: Field -> Value -> Parser Expr
fieldLogic "$not" expr = ExprNOT <$> query expr
fieldLogic op expr = withArray "Logical operator" f expr where
  f a = case op of
    "$or" -> foldOp ExprOR <$> mapM query (V.toList a)
    "$and" -> foldOp ExprAND <$> mapM query (V.toList a)
    "$nor" -> ExprNOT . foldOp ExprAND <$> mapM query (V.toList a)
    _ -> empty

-- TODO : Fix for empty expression list: make total
foldOp :: (Expr -> Expr -> Expr) -> [Expr] -> Expr
foldOp op [expr] = expr
foldOp op (expr : exprs) = op expr $ foldOp op exprs


-- | Build an expression of field constraints from a JSON object
fieldConstraints :: Field -> Value -> Parser Expr
fieldConstraints field (Object o) = build (H.toList o) where
  build [] = empty
  build [(k, v)] = fieldConstraint field k v
  build ((k, v) : kvs) = ExprAND <$> fieldConstraint field k v
                                 <*> build kvs

fieldConstraint :: Field -> T.Text -> Value -> Parser Expr
fieldConstraint field "$nin" v = ExprNOT . ExprConstr field <$> constraint "$in" v
fieldConstraint field "$all" (Array a) = pure $ foldOp ExprAND $ map f (V.toList a) where
  f v = ExprConstr field $ constraint v
fieldConstraint field k v      = ExprConstr field <$> constraint k v

constraint :: T.Text  -> Value -> Parser FieldConstraint
constraint "$eq" v            = ConstrEQ <$> pure v
constraint "$lt" v            = ConstrLT <$> pure v
constraint "$gt" v            = ConstrGT <$> pure v
constraint "$lte" v           = ConstrLE <$> pure v
constraint "$gte" v           = ConstrGE <$> pure v
constraint "$in" (Array a)    = ConstrIN <$> pure (V.toList a)
-- TODO expand: constraint "$nin" (Array a) = ExprNIN field <$> pure a
-- TODO expand: constraint "$all" (Array a) = ExprALL field <$> pure a
constraint "$exists" (Bool b) = ConstrEXISTS <$> pure b
constraint "$type" (Number x) = ConstrTYPE <$> case S.toBoundedInteger x of
    Nothing -> empty
    Just i  -> pure $ intToBsonType i
constraint "$size" (Number x) = ConstrSIZE <$> case S.toBoundedInteger x of
    Nothing -> empty
    Just i  -> pure i
constraint "$elemMatch" (Array a) = ConstrEMATCH <$> mapM query (V.toList a)
constraint _ _ = empty

{- TODO
  | ExprMOD Field Int Int
  | ExprREGEX Field T.Text (Maybe T.Text)
  | ExprTEXT Field T.Text (Maybe T.Text)
-}
