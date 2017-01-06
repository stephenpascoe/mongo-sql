module Main where

import Data.Maybe

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as E
import qualified Data.Aeson as A
import System.Console.Haskeline
import Control.Monad.Trans

import Talos.Lib

type Repl a = InputT IO a

main :: IO ()
main = runInputT defaultSettings repl

repl :: Repl ()
repl = do
  minput <- getInputLine "talos> "
  case minput of
    Nothing -> outputStrLn "Goodbye."
    Just input -> liftIO (process (BL.pack input)) >> repl

process :: BL.ByteString -> IO ()
process line =
  let maybeExpr = do
        expr <- A.decode line
        findToSqlText expr
  in case maybeExpr of
    Nothing -> TIO.putStrLn $ T.concat ["ERROR: Doesn't parse!"]
    Just sqlTxt -> TIO.putStrLn sqlTxt
