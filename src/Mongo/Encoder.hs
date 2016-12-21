module Mongo.Encoder where

import Data.Aeson
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Scientific as S
import qualified Data.HashMap.Strict as H
import Control.Monad
import Data.Foldable
import qualified Data.Bson as B

import Types


instance ToJSON QueryExpr where
  toJSON = unparse

intToScientific :: Int -> S.Scientific
intToScientific x = S.scientific (toInteger x) 0

mkOp :: T.Text -> Field -> B.Value -> Value
mkOp op lhs rhs = object [ lhs .= object [op .= aesonifyValue rhs] ]

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


unparse :: QueryExpr -> Value
unparse (ExprNOT (ExprConstr (OpEQ field value))) = mkOp "$ne" field value
unparse (ExprNOT (ExprConstr (OpIN field lst))) = mkOp "$nin" field $ B.Array lst
unparse (ExprAND exprs) = object [ "$and" .= foldExprs exprs ]
unparse (ExprOR exprs) = object [ "$or" .= foldExprs exprs ]
unparse (ExprNOT (ExprOR exprs)) = object [ "$nor" .= foldExprs exprs ]
unparse (ExprNOT expr) = object [ "$not" .= toJSON expr ]
unparse (ExprConstr op) = unparseConstr op

unparseConstr :: QueryOp -> Value
unparseConstr (OpEQ field value) = object [ field .= aesonifyValue value ]
unparseConstr (OpLT field value) = mkOp "$lt" field value
unparseConstr (OpLE field value) = mkOp field "$lte" value
unparseConstr (OpGT field value) = mkOp "$gt" field value
unparseConstr (OpGE field value) = mkOp "$gte" field value
unparseConstr (OpIN field lst) = mkOp "$in" field (B.Array lst)
unparseConstr (OpEMATCH field exprs) = object ["$elemMatch" .= fmap unparse exprs]
unparseConstr (OpSIZE field i) = object [field .= object ["$size" .= (Number $ S.scientific i 0)]]
unparseConstr (OpTYPE field i) = object [field .= object ["$type" .= (Number $ S.scientific (bsonTypeToInt i) 0)]]

-- TODO : merge fields
-- TODO : gather fields with same name to make $all claues
{-
unparse (OpALL field array) = mkOp "$all" field (Array array)
-}

-- | Convert a BSON value into an Aeson value
-- TODO : This function is not total.  Deprecated, just use until we move to JSON parser
aesonifyValue :: B.Value -> Value
aesonifyValue (B.Float f) = toJSON f
aesonifyValue (B.String s) = toJSON s
aesonifyValue (B.Doc doc) = Object asAeson where
  f ((B.:=) l v) = (l, aesonifyValue v)
  asAeson = H.fromList . fmap f $ doc
aesonifyValue (B.Array list) = Array . V.fromList $ fmap aesonifyValue list
aesonifyValue (B.Bool bool) = toJSON bool
aesonifyValue (B.UTC utc) = toJSON utc
aesonifyValue (B.Null) = Null
{- TODO REGEX?
aesonifyValue (RegEx (Regex pattern mods)) = toJSON $
                                           '/' : T.unpack pattern ++
                                           '/' : T.unpack mods
-}
aesonifyValue (B.Int32 int32) = toJSON int32
aesonifyValue (B.Int64 int64) = toJSON int64
aesonifyValue _ = error "Unsupported BSON type"
