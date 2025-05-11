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
import System.Console.ANSI ()

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
                putStrLn $ "Error: " ++ show err
                return ()
            Right (_, _, _, pid) -> do
                exitCode <- waitForProcess pid
                case exitCode of
                    ExitSuccess   -> return ()
                    ExitFailure _ -> putStrLn "Command fail."

textCol :: String -> String -> IO ()
textCol code name = putStr $ "\ESC[" ++ code ++ "m" ++ name ++ "\ESC[0m"
inColor :: String -> String -> IO String
inColor colorCode promptText = do
  putStr $ "\ESC[" ++ colorCode ++ "m" ++ promptText
  input <- getLine
  putStr "\ESC[0m" 
  return input

-- Shell Text
shellText :: IO ()
shellText = do
  currentDir <- getCurrentDirectory
  username <- getEnv "USER"
  -- COLORED SHELL TEXT
  textCol "91" (username ++ " ")
  textCol "32" (currentDir ++ " ")
  textCol "95" "$ "
  hFlush stdout


-- COLOR CODES
{-
0   - default
30  - black
31  - red
32  - green
33  - yellow
34  - blue
35  - magenta
36  - cyan
37  - white
90  - bright black (gray)
91  - bright red
92  - bright green
93  - bright yellow
94  - bright blue
95  - bright magenta
96  - bright cyan
97  - bright white
-}

-- TODO : Create a configuration file.
-- TODO : Colorize output.
-- TODO : Optimize shell.
-- TODO : Create theming config.
