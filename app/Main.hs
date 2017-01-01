module Main where

import Data.Maybe

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as E

import Talos.Lib

main :: IO ()
main = repl

repl :: IO ()
repl = do
  TIO.putStrLn "talos> "
  line <- TIO.getLine
  processLine (BL.fromStrict $ E.encodeUtf8 line)
  repl

processLine :: BL.ByteString -> IO ()
processLine line = undefined
