module Talos.Mongo.Encoder where

import Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Scientific as S
import qualified Data.HashMap.Strict as H
import Control.Monad
import Data.Foldable
import qualified Data.Bson as B

import Talos.Types


instance ToJSON QueryExpr where
  toJSON = aesonify

intToScientific :: Int -> S.Scientific
intToScientific x = S.scientific (toInteger x) 0

mkOp :: T.Text -> Field -> DocValue -> A.Value
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


aesonify :: QueryExpr -> A.Value
aesonify (ExprNOT (ExprConstr (OpEQ field value))) = mkOp "$ne" field value
aesonify (ExprNOT (ExprConstr (OpIN field lst))) = mkOp "$nin" field $ B.Array lst
aesonify (ExprAND exprs) = object [ "$and" .= foldExprs exprs ]
aesonify (ExprOR exprs) = object [ "$or" .= foldExprs exprs ]
aesonify (ExprNOT (ExprOR exprs)) = object [ "$nor" .= foldExprs exprs ]
aesonify (ExprNOT expr) = object [ "$not" .= toJSON expr ]
aesonify (ExprConstr op) = aesonifyConstr op

aesonifyConstr :: QueryOp -> A.Value
aesonifyConstr (OpEQ field value) = object [ field .= aesonifyValue value ]
aesonifyConstr (OpLT field value) = mkOp "$lt" field value
aesonifyConstr (OpLE field value) = mkOp field "$lte" value
aesonifyConstr (OpGT field value) = mkOp "$gt" field value
aesonifyConstr (OpGE field value) = mkOp "$gte" field value
aesonifyConstr (OpIN field lst) = mkOp "$in" field (B.Array lst)
aesonifyConstr (OpEMATCH field exprs) = object ["$elemMatch" .= fmap aesonify exprs]
aesonifyConstr (OpSIZE field i) = object [field .= object ["$size" .= (Number $ S.scientific i 0)]]
aesonifyConstr (OpTYPE field i) = object [field .= object ["$type" .= (Number $ S.scientific (bsonTypeToInt i) 0)]]

-- TODO : merge fields
-- TODO : gather fields with same name to make $all claues
{-
aesonify (OpALL field array) = mkOp "$all" field (Array array)
-}

-- | Convert a BSON value into an Aeson value
-- TODO : This function is not total.  Deprecated, just use until we move to JSON parser
aesonifyValue :: DocValue -> A.Value
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
