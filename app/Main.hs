module Main where

import Data.Either
import Data.Maybe
import Json
import Parsing (runParser)
import System.Environment
import System.Exit hiding (die)
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
version = putStrLn "Haskell ft_turing 0.1.1.0"

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
      this = mapM_ (\a -> putStr "\t" >> print a) $ concatMap (map fromObject . fromArray . snd) filtered
      next = fromStarting obj xs
   in displayStarting >> this >> next
fromStarting _ [] = return ()

runMachine :: Machine -> IO ()
runMachine m =
  let printState = putStr . showTape . currentState $ m
      matchingTransition = filter (not . null . (`transition` currentState m)) . transitions $ m
      printTransition = print . head $ matchingTransition
      doTransition = Turing.iterate m
      step = printState >> printTransition
      finale = putStrLn "Stopping condition met"
   in case doTransition of
        Right newM -> step >> runMachine newM
        Left s
          | s == "final" -> step >> finale
          | otherwise -> putStrLn s

main :: IO ()
main = do
  (json, input) <- parse =<< getArgs
  let obj = snd . fromMaybe ("", Null) . runParser jsonValue $ json
  let machine = createMachine obj input
  if isLeft . isJsonValid $ obj
    then
      putStrLn . fromLeft "" . isJsonValid $ obj
    else
      if isLeft . isValid $ machine
        then putStrLn . fromLeft "" . isValid $ machine
        else print machine >> runMachine machine

exit :: IO a
exit = exitSuccess

die :: IO a
die = exitFailure
