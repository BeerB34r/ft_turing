module Main where

import Data.List

createTransition :: Char -> Char -> Char -> Char -> Char -> String
createTransition initial r next write action = ['[', initial, r, next, write, action, ']']

scanright :: String -> String -> String
scanright states alpha =
  let base = createTransition (head states)
      first = base (head alpha) (head states) (head alpha) '>'
      second = base (alpha !! 1) (states !! 1) (alpha !! 2) '>'
      third = base (alpha !! 2) (states !! 2) (alpha !! 2) '&'
   in first ++ second ++ third

swap :: String -> String -> String
swap states alpha =
  let base = createTransition (states !! 1)
      first = base (head alpha) (states !! 2) (alpha !! 1) '<'
      second = base (alpha !! 2) (states !! 1) (alpha !! 2) '&'
   in first ++ second

createInput :: String -> String
createInput (a : b : c : _) = [a, a, a, b, a, a, c]
createInput _ = undefined

createTape :: String -> String -> String
createTape states alpha =
  "^"
    ++ [head states]
    ++ "%"
    ++ scanright states alpha
    ++ swap states alpha
    ++ createTransition (states !! 2) (alpha !! 2) (head states) (head alpha) '>'
    ++ "$"
    ++ createInput alpha

main :: IO ()
main = mapM_ putStrLn (concatMap ((`map` permutations "abc") . createTape) (permutations "ABC"))
