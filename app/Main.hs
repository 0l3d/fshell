module Main where

import qualified Shell (initShell, shellText)
import System.IO
import Control.Monad (when)

shellLoop :: IO ()
shellLoop = do
  Shell.shellText
  command <- getLine
  let n = 0
  when (n < 1) $ do
    Shell.initShell command
    shellLoop


main :: IO ()
main = do
  shellLoop
  putStrLn "Bye"
