{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Main where

import qualified Shell (initShell, shellText)
import System.IO ()
import Control.Monad (when, forever)
import System.Posix.Signals
    ( installHandler, Handler(Catch), sigINT)

shellLoop :: IO ()
shellLoop = forever $ do
  Shell.shellText
  command <- getLine
  let interrupt = do
       putStrLn "^C (you cant turn off me!!!)"
  installHandler sigINT (Catch interrupt) Nothing
  if command == "" then
    shellLoop
  else do
    Shell.initShell command

main :: IO ()
main = do
  shellLoop
