module Main where

import Data.Maybe

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as E
import qualified Data.Aeson as A
import System.Console.Haskeline
import Control.Monad.Trans

import Transfuser.Lib

type Repl a = InputT IO a

main :: IO ()
main = runInputT defaultSettings repl

repl :: Repl ()
repl = do
  minput <- getInputLine "transfuse> "
  case minput of
    Nothing -> outputStrLn "Goodbye."
    Just input -> liftIO (process (BL.pack input)) >> repl

process :: BL.ByteString -> IO ()
process line =
  let maybeExpr = do
        expr <- A.eitherDecode line
        findToSqlText expr
  in case maybeExpr of
    Left err -> TIO.putStrLn $ T.concat ["ERROR: ", T.pack err]
    Right sqlTxt -> TIO.putStrLn sqlTxt
