{-# LANGUAGE QuasiQuotes #-}

module Main where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances

import Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Bson as B
import Text.RawString.QQ
import Data.Maybe

import Talos.Lib
import Talos.Types
import Talos.Types.Arbitrary
import Talos.Sql.Encoder


main :: IO ()
main = hspec $ do
  describe "MongoDB JSON query language" $ do
    -- General parsing
    it "Any query expression will encode to SQL AST" $ property $
      \expr -> isJust $ findToSqlText expr
    it "Any find expression will encode to SQL" $ property $
      \expr -> isJust $ queryToSQL expr
    -- Specific examples
    it "Example query 1 parses" $ do
      isJust (decode eg1 :: Maybe QueryExpr) `shouldBe` True
    it "Example find expression 1 parses" $ do
      isJust (decode eg2 :: Maybe FindExpr) `shouldBe` True
    -- Single expression decoding
    it "encodes default equality" $ property $
      \a b -> fromJSON (object [ a .= b ]) == A.Success (ExprConstr $ OpEQ a (B.String b))
    it "encodes equality with $eq" $ property $
      \a b -> fromJSON (object [ a .= object [ "$eq" .= b ] ])
              == A.Success (ExprConstr $ OpEQ a (B.String b))
    it "encodes logical and" $ property $
      \a b c d -> fromJSON (object [ "$and" .= [object [a .= b], object [c .= d]] ])
                  == A.Success (ExprAND [ ExprConstr $ OpEQ a (B.String b)
                                        , ExprConstr $ OpEQ c (B.String d)
                                        ])

------------------------------------------------------------------------------

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

eg2 = [r|
{
  "collection": "col1",
  "projection": {"a": 1, "b": 1, "c": 1},
  "query": {"$or": [
             {"context_tags": {"$exists": false}},
             {"$and": [
               {"context_tags.department": "qc"},
               {"context_tags.experiment_type": "full_pore_insertion"}
             ]}
           ]}
}
|]
