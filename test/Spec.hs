{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
import Test.QuickCheck
import Lib

import qualified Data.Aeson as A
import Data.Aeson.Types (Parser, parse)
import Data.Aeson ((.:), (.=))
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H
import Data.Scientific as S


prop_eq :: String -> String -> Bool
prop_eq a b = parse query (A.object [ a' .= b' ]) == A.Success (ExprEQ a' (A.String b')) where
  a' = T.pack a
  b' = T.pack a

prop_eq2 :: String -> String -> Bool
prop_eq2 a b =
  parse query (A.object [ a' .= A.object [ "$eq" .= b' ] ]) == A.Success (ExprEQ a' (A.String b')) where
    a' = T.pack a
    b' = T.pack a


prop_and1 :: String -> String -> String -> String -> Bool
prop_and1 a b c d =
  parse query (A.object [ "$and" .= [A.object [a' .= b'], A.object [c' .= d']] ]) == A.Success (ExprAND [ExprEQ a' (A.String b'), ExprEQ c' (A.String d')]) where
    a' = T.pack a
    b' = T.pack b
    c' = T.pack c
    d' = T.pack d


main :: IO ()
main = hspec $ do
  describe "MongoDB JSON query language" $ do
    it "encodes default equality" $ property prop_eq
    it "encodes equality with $eq" $ property prop_eq2
    it "encodes logical and" $ property prop_and1
