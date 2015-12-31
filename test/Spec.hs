module Main where

import Test.Hspec
import Test.QuickCheck
import Lib

import Data.Aeson as A
import qualified Data.Text as T

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary
  shrink xs = T.pack <$> shrink (T.unpack xs)


fromJSON_eq :: Value -> Expr -> Bool
fromJSON_eq obj expr = fromJSON obj == A.Success expr

-- toJSON_eq :: Expr -> Value -> Bool
-- toJSON_eq expr obj = toJSON expr == obj


prop_eq1 :: T.Text -> T.Text -> Bool
prop_eq1 a b = fromJSON_eq (object [ a .= b ]) (ExprEQ a (String b))


prop_eq2 :: T.Text -> T.Text -> Bool
prop_eq2 a b = fromJSON_eq (object [ a .= object [ "$eq" .= b ] ]) (ExprEQ a (String b))

prop_and1 :: T.Text -> T.Text -> T.Text -> T.Text -> Bool
prop_and1 a b c d = fromJSON_eq (object [ "$and" .= [object [a .= b], object [c .= d]] ])
                                (ExprAND [ExprEQ a (String b), ExprEQ c (String d)])

main :: IO ()
main = hspec $ do
  describe "MongoDB JSON query language" $ do
    it "encodes default equality" $ property prop_eq1
    it "encodes equality with $eq" $ property prop_eq2
    it "encodes logical and" $ property prop_and1
