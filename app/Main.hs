module Main where

import Control.Monad
import Data.Either
import Data.Maybe
import Json
import Parsing (runParser)
import System.Environment
import System.Exit hiding (die)
import System.IO
import Turing

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
version = putStrLn "Haskell ft_turing 0.2.0.0"

parse :: [String] -> IO (String, String)
parse ["--help"] = help >> exit
parse ["-h"] = help >> exit
parse ["--version"] = version >> exit
parse ["-v"] = version >> exit
parse [json, input] =
  (\a -> (head a, a !! 1))
    <$> sequence [readFile json, return input :: IO String]
parse _ = usage >> die

putJson :: String -> IO ()
putJson = print . snd . fromMaybe ("", Object []) . runParser jsonValue

putVal :: (Show a) => JsonValue -> String -> (JsonValue -> a) -> IO ()
putVal obj key f =
  let title = putStr . capitalise $ key ++ ": "
      value = print . f . head . getValue obj $ key
   in title >> value

fromStarting :: [(JsonValue, JsonValue)] -> [String] -> IO ()
fromStarting obj (x : xs) =
  let displayStarting = putStrLn x
      filtered = filter ((== x) . fromString . fst) obj
      this = mapM_ (\a -> putStr "\t" >> print a) . concatMap (map fromObject . fromArray . snd) $ filtered
      next = fromStarting obj xs
   in displayStarting >> this >> next
fromStarting _ [] = return ()

runMachine :: Machine -> IO ()
runMachine m =
  let printState = putStr . showTape . currentState $ m
      matchingTransition = filter (not . null . (`transition` currentState m)) . transitions $ m
      printTransition = print . head $ matchingTransition
      doTransition = Turing.iterate m
      step =
        printState
          >> putStr " -> "
          >> (putStr . showTape . currentState . fromRight m $ doTransition)
          >> putStr " "
          >> printTransition
      finale = putStrLn "Stopping condition met"
   in case doTransition of
        Right newM
          | (state . currentState $ newM) `notElem` finals newM -> step >> runMachine newM
          | otherwise -> step >> finale
        Left s -> putStrLn s

main :: IO ()
main =
  let args = parse =<< getArgs
      input = snd <$> args
      obj = snd . fromMaybe ("", Null) . runParser jsonValue . fst <$> args
      checkJson = isLeft . isJsonValid
      checkMachine = isLeft . isValid
      putErr = hPutStrLn stderr . fromLeft ""
      run o i
        | checkJson o = putErr . isJsonValid $ o
        | checkMachine (createMachine o i) = putErr . isValid $ createMachine o i
        | otherwise = print (createMachine o i) >> runMachine (createMachine o i)
   in join ((run <$> obj) <*> input)

exit :: IO a
exit = exitSuccess

die :: IO a
die = exitFailure
