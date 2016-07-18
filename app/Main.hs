module Main where

import Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import Lib

-- Return an error or string to print.
parseLine :: BL.ByteString -> A.Result Expr
parseLine str = case A.decode str of
  Nothing -> Error "ERROR"
  Just obj -> A.fromJSON obj

main = undefined
{-
main :: IO ()
main = do
  putStr "> "
  line <- BL.readLn
  case parseLine line of
    A.Error message -> putStrLn $ "ERROR: " ++ message
    A.Success expr -> putStrLn $ show expr
-}
