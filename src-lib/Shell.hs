{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Shell (initShell, shellText) where

import System.Exit (ExitCode (..), exitSuccess)
import System.Directory (setCurrentDirectory, getCurrentDirectory)
import System.Environment (getEnv)
import System.Process
    ( createProcess,
      proc,
      waitForProcess,
      CreateProcess(std_err, std_in, std_out),
      StdStream(CreatePipe, Inherit), ProcessHandle )
import System.IO ( stdout, hFlush, hGetContents, Handle )
import Control.Exception ( SomeException, try )

-- Shell initialize.
initShell :: String -> IO ()
initShell command = do
    if command == "exit" then do
      putStrLn "Bye!"
      exitSuccess
    else if take 2 command == "cd" then do
        let dir = drop 3 command
        setCurrentDirectory dir
    else do
        let args = words command
        let cmd = head args
        let cmdArgs = tail args

        let processSpec = (proc cmd cmdArgs) { std_out = Inherit, std_err = Inherit, std_in = Inherit }

        result <- try (createProcess processSpec) :: IO (Either SomeException (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle))
        case result of
            Left err -> do
                putStrLn $ "Hata: " ++ show err
                return ()
            Right (_, _, _, pid) -> do
                exitCode <- waitForProcess pid
                case exitCode of
                    ExitSuccess   -> return ()
                    ExitFailure _ -> putStrLn "Command fail."

shellText :: IO ()
shellText = do
  currentDir <- getCurrentDirectory
  username <- getEnv "USER"
  putStr (username ++ " " ++ currentDir ++ " $ ")
  hFlush stdout

-- TODO : Create a configuration file.
-- TODO : Colorize output.
-- TODO : Optimize shell.
-- TODO : Create theming config.
