module Main where

import Data.Maybe
import Json
import Parsing (runParser)
import System.Environment
import System.Exit

usage :: IO ()
usage = putStrLn "Usage: ft_turing [-vh|--help|--version] <machine-description.json>"

version :: IO ()
version = putStrLn "Haskell ft_turing 0.0.1.0"

parse :: [String] -> IO String
parse ["--help"] = usage >> exit
parse ["-h"] = usage >> exit
parse ["--version"] = version >> exit
parse ["-v"] = version >> exit
parse [] = getContents
parse fs = concat `fmap` mapM readFile fs

putJson :: String -> IO ()
putJson = print . snd . fromMaybe ("", Object []) . runParser jsonValue

main :: IO ()
main = do
  args <- getArgs
  parseRes <- parse args
  putJson parseRes
  putStrLn "Goodbye"

exit :: IO a
exit = exitSuccess

die :: IO a
die = exitFailure
