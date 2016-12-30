{-# LANGUAGE OverloadedStrings #-}


module Talos.Types.Arbitrary () where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import qualified Data.Text as T
import qualified Data.Bson as B

import Talos.Types

-- scale argibrary values to make them readable
textScale = scale (const 8)
seqScale = scale (const 3)

instance Arbitrary FindExpr where
  arbitrary = FindExpr <$> genSafeText <*> arbitrary <*> arbitrary

instance Arbitrary Projection where
  arbitrary = seqScale $ Projection <$> ops where
    ops = listOf (ProjInclude <$> genSafeText)

instance Arbitrary QueryExpr where
  arbitrary = seqScale $ frequency [ (6, ExprConstr <$> arbitrary)
                                   , (1, ExprOR <$> listOf1 arbitrary)
                                   , (1, ExprAND <$> listOf1 arbitrary)
                                   , (1, ExprNOT <$> arbitrary)
                                   ]

instance Arbitrary B.Field where
  arbitrary = (B.:=) <$> genSafeText <*> arbitrary

instance Arbitrary B.Value where
  arbitrary = seqScale $ frequency [ (3, B.Float <$> arbitrary)
                                   , (3, B.String <$> genSafeText)
                                   , (1, B.Doc <$> arbitrary)
                                   , (1, B.Array <$> arbitrary)
                                     -- TODO : Other BSON types
                                   ]

instance Arbitrary QueryOp where
  arbitrary = seqScale $ oneof [ OpEQ <$> genSafeText <*> arbitrary
                               , OpLT <$> genSafeText <*> arbitrary
                               , OpGT <$> genSafeText <*> arbitrary
                               , OpLE <$> genSafeText <*> arbitrary
                               , OpGE <$> genSafeText <*> arbitrary
                               , OpIN <$> genSafeText <*> arbitrary
                               , OpMOD <$> genSafeText <*> arbitrary <*> arbitrary
                                 -- TODO : Not generated: OpREGEX OpTEXT OpEMATCH
                                 -- TODO : Not generated: OpSIZE OpEXISTS OpTYPE
                               ]


genSafeText = textScale $ T.pack <$> chars where
  chars = listOf genSafeChar
  genSafeChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']

genId = do
  text <- genSafeText
  startId <- elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['_']
  let name = T.cons startId text
  return name
