module Main where

import Data.Maybe
import Json
import Parsing (runParser)
import System.Environment
import System.Exit hiding (die)

usage :: IO ()
usage = putStrLn "Usage: ft_turing [-vh|--help|--version] <machine-description.json> <input>"

help :: IO ()
help =
  usage
    >> putStrLn "positional arguments:"
    >> putStrLn "\tmachine-description.json\t\tjson description of the turing machine\n"
    >> putStrLn "\tinput\t\t\t\tinitial state of the machine's tape\n"
    >> putStrLn "optional arguments:"
    >> putStrLn "\t-h, --help\t\t\tshow this help message and exit\n"
    >> putStrLn "\t-v, --version\t\t\tshow program version and exit"

version :: IO ()
version = putStrLn "Haskell ft_turing 0.1.0.2"

parse :: [String] -> IO [String]
parse ["--help"] = help >> exit
parse ["-h"] = help >> exit
parse ["--version"] = version >> exit
parse ["-v"] = version >> exit
parse [json, input] = sequence [readFile json, return input :: IO String]
parse _ = usage >> die

putJson :: String -> IO ()
putJson = print . snd . fromMaybe ("", Object []) . runParser jsonValue

main :: IO ()
main = do
  args <- getArgs
  [json, input] <- parse args
  putJson json
  putStr "tape: "
  print input
  putStrLn "Goodbye"

exit :: IO a
exit = exitSuccess

die :: IO a
die = exitFailure
