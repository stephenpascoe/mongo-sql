module Mongo.Encoder where

import Data.Aeson
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Scientific as S
import qualified Data.HashMap.Strict as H
import Control.Monad
import Data.Foldable

import Types


instance ToJSON Expr where
  toJSON = unparse

intToScientific :: Int -> S.Scientific
intToScientific x = S.scientific (toInteger x) 0

mkOp :: T.Text -> Field -> Value -> Value
mkOp op lhs rhs = object [ lhs .= object [op .= rhs] ]

-- | Fold a list of field expressions into a single object
foldExprs = foldFields . fmap toJSON where
  foldFields objs = do
    let
      f (Object a) = Just a
      f _ = Nothing
    hmaps <- mapM f objs
    let
      allValues = join $ map H.toList hmaps
    return (Object $ H.fromList allValues)


unparse :: Expr -> Value
unparse (ExprNOT (ExprConstr (OpEQ field value))) = mkOp "$ne" field value
unparse (ExprNOT (ExprConstr (OpIN field lst))) = mkOp "$nin" field (Array $ V.fromList lst)
unparse (ExprAND exprs) = object [ "$and" .= foldExprs exprs ]
unparse (ExprOR exprs) = object [ "$or" .= foldExprs exprs ]
unparse (ExprNOT (ExprOR exprs)) = object [ "$nor" .= foldExprs exprs ]
unparse (ExprNOT expr) = object [ "$not" .= toJSON expr ]
unparse (ExprConstr op) = unparseConstr op

unparseConstr :: FieldOp -> Value
unparseConstr (OpEQ field value) = object [ field .= value ]
unparseConstr (OpLT field value) = mkOp "$lt" field value
unparseConstr (OpLE field value) = mkOp field "$lte" value
unparseConstr (OpGT field value) = mkOp "$gt" field value
unparseConstr (OpGE field value) = mkOp "$gte" field value
unparseConstr (OpIN field lst) = mkOp "$in" field (Array $ V.fromList lst)
unparseConstr (OpEMATCH field exprs) = mkOp "$elemMatch" field (Array . V.fromList $ fmap toJSON exprs)
unparseConstr (OpSIZE field i) = mkOp "$size" field (Number $ S.scientific i 0)
unparseConstr (OpTYPE field i) = mkOp "$type" field (Number $ S.scientific (bsonTypeToInt i) 0)

-- TODO : merge fields
-- TODO : gather fields with same name to make $all claues
{-
unparse (OpALL field array) = mkOp "$all" field (Array array)
-}
