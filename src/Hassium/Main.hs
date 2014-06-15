module Hassium.Main where

import Control.Monad(liftM, when)
import Data.Maybe(isJust, fromJust)
import System.Environment(getArgs)
import System.Exit(exitWith, ExitCode(ExitFailure))
import System.IO(readFile)
import Hassium.Args(parseOpt, Flag(..))

_main :: IO ()
_main = do
  parsedArgs <- liftM parseOpt getArgs
  (flags, filePath) <- case parsedArgs of
    Right (flags, filePath) -> do
      when (Verbose `elem` flags) $
        putStrLn $ "options: " ++ (show flags) ++ "\n"
      when (not $ isJust filePath) $ do
        putStrLn "no file path specification"
        exitWith $ ExitFailure 1
      return (flags, fromJust filePath)
    Left e -> do
      putStrLn $ show e
      exitWith $ ExitFailure 1
  fileContext <- readFile filePath
--  compile flags fileContext  
  return ()


-- compile :: String -> [Flags] -> IO ()
-- compile fullName flags =

-- (filePath, moduleName, extension) =
--        (takeDirectory filePath, takeBaseName filePath, takeExtension filePath)
