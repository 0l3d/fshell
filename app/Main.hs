{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Main where

import qualified Shell (initShell, shellText)
import System.IO ()
import Control.Monad (forever)
import System.Console.Haskeline
    ( defaultSettings, getInputLine, runInputT, InputT )
import System.Posix.Signals
    ( installHandler, Handler(Catch), sigINT)
import Control.Monad.IO.Class (MonadIO(liftIO))

shellLoop :: InputT IO ()
shellLoop = forever $ do
  liftIO Shell.shellText
  command <- getInputLine ""
  case command of
    Nothing -> return()
    Just comm -> do
      liftIO $ Shell.initShell comm
  let interrupt = do
       putStrLn "^C (you cant turn off me!!!)"
  liftIO $ installHandler sigINT (Catch interrupt) Nothing


main :: IO ()
main = do
  runInputT defaultSettings shellLoop
