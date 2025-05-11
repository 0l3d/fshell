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
    let n = if command == "exit" then 1 else 0
    let commandList = words command
    Shell.initShell commandList
    shellLoop


main :: IO ()
main = do
  shellLoop
  putStrLn "Bye"
