module Main where

import Lib

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
