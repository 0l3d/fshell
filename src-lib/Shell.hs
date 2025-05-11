module Shell (initShell, shellText) where

import System.Exit ()
import System.Directory (setCurrentDirectory, getCurrentDirectory)
import System.Environment (getEnv)
import System.Process
import System.IO
import GHC.Arr (safeIndex)
import Text.Printf (printf)

-- Shell initialize.
initShell :: [String] -> IO ()
initShell str = do
  let (command:arguments) = str
  if command == "cd"
    then do
      let directory =  if length arguments > 0 then Just (arguments !! 0) else Nothing
      case directory of
        Just path -> do
          setCurrentDirectory path
        Nothing -> putStrLn "no such file or directory"
    else do
      (Just hin, Just hout, Just herr, _) <- createProcess (proc command arguments)
        { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
      output <- hGetContents hout
      putStrLn output

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
