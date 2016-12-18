{-# LANGUAGE QuasiQuotes #-}

module MongoSQL.Test where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances
import Lib

import Data.Aeson as A
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import Text.RawString.QQ


fromJSON_eq :: Value -> Expr -> Bool
fromJSON_eq obj expr = fromJSON obj == A.Success expr

json_eq :: BL.ByteString -> Expr -> Bool
json_eq str expr = case decode str of
  Nothing -> False
  Just obj -> obj == expr

-- toJSON_eq :: Expr -> Value -> Bool
-- toJSON_eq expr obj = toJSON expr == obj


prop_eq1 :: T.Text -> T.Text -> Bool
prop_eq1 a b = fromJSON_eq (object [ a .= b ]) (ExprEQ a (String b))


prop_eq2 :: T.Text -> T.Text -> Bool
prop_eq2 a b = fromJSON_eq (object [ a .= object [ "$eq" .= b ] ]) (ExprEQ a (String b))


prop_and1 :: T.Text -> T.Text -> T.Text -> T.Text -> Bool
prop_and1 a b c d = fromJSON_eq (object [ "$and" .= [object [a .= b], object [c .= d]] ])
                                (ExprAND [ExprEQ a (String b), ExprEQ c (String d)])


eg1 = [r|{"$and": [
           {"tracking_id.asic_id": "AAAA"},
           {"membrane_summary": {"$exists": true}},
           {"$or": [
             {"context_tags": {"$exists": false}},
             {"$and": [
               {"context_tags.department": "qc"},
               {"context_tags.experiment_type": "full_pore_insertion"}
             ]}
           ]}
         ]}
|]

main :: IO ()
Main = hspec $ do
  describe "MongoDB JSON query language" $ do
    it "encodes default equality" $ property prop_eq1
    it "encodes equality with $eq" $ property prop_eq2
    it "encodes logical and" $ property prop_and1
