{-# LANGUAGE OverloadedRecordDot #-}

module Turing where

import Data.List
import Json

data Machine = Machine
  { name :: String,
    alphabet :: String,
    blank :: Char,
    states :: [String],
    initialState :: String,
    finals :: [String],
    transitions :: [Transition],
    currentState :: State
  }

instance Show Machine where
  show m =
    let showName = show m.name ++ ":\n"
        showAlphabet = "\tAlphabet: " ++ show m.alphabet ++ "\n"
        showBlank = "\tBlank: " ++ show m.blank ++ "\n"
        showStates = "\tStates: [\n" ++ concatMap ((++ "\n") . ("\t\t" ++) . show) m.states ++ "]\n"
        showInitial = "\tInitial: " ++ (show . initialState $ m) ++ "\n"
        showFinals = "\tFinals: [\n" ++ concatMap ((++ "\n") . ("\t\t" ++) . show) m.finals ++ "]\n"
        showTransitions = "\tTransitions: [\n" ++ concatMap ((++ "\n") . ("\t\t" ++) . show) m.transitions ++ "]\n"
        showCurrentState = "\tCurrent state: " ++ show m.currentState ++ "\n"
     in showName ++ showAlphabet ++ showBlank ++ showStates ++ showInitial ++ showFinals ++ showTransitions ++ showCurrentState

data State = State
  { state :: String,
    tape :: (Int, String)
  }

showTape :: State -> String
showTape = ("[" ++) . (++ "]") . (\(a, b) -> a ++ "<" ++ [head b] ++ ">" ++ tail b) . uncurry splitAt . tape

showState :: State -> String
showState = show . state

instance Show State where
  show s = showState s ++ " " ++ showTape s

current :: State -> Char
current = uncurry (flip (!!)) . tape

data Transition = Transition
  { transition :: State -> [State],
    description :: String
  }

instance Show Transition where
  show = description

createTransition :: String -> Char -> String -> Char -> String -> Transition
createTransition initState c nextState w action =
  let sameState = (== initState) . state
      sameChar = (== c) . current
      isTransitionValid s = sameState s && sameChar s
      moveHead i = i + (if action == "LEFT" then -1 else 1)
      modifyTape s i = take i s ++ [w] ++ drop (i + 1) s
      doTransition s =
        ( [ State
              { state = nextState,
                tape = (moveHead . fst . tape $ s, uncurry (flip modifyTape) . tape $ s)
              }
            | (moveHead . fst . tape $ s) < (length . snd . tape $ s)
          ]
        )
   in Transition
        { transition = \s -> if isTransitionValid s then doTransition s else [],
          description =
            show initState
              ++ " -> "
              ++ show nextState
              ++ ": '"
              ++ [c]
              ++ "' -> '"
              ++ [w]
              ++ "', "
              ++ action
        }

createTransitions :: [String] -> JsonValue -> [Transition]
createTransitions (x : xs) (Object obj) =
  let currentTransitions = concatMap (fromArray . snd) . filter ((== x) . fromString . fst) $ obj -- [jsonObject]
      createSingle o =
        createTransition
          x
          (head . fromString . head . getValue o $ "read")
          (fromString . head . getValue o $ "to_state")
          (head . fromString . head . getValue o $ "write")
          (fromString . head . getValue o $ "action")
   in map createSingle currentTransitions ++ createTransitions xs (Object obj)
createTransitions _ _ = []

createMachine :: JsonValue -> String -> Machine
createMachine obj initialTape =
  let getVal = head . getValue obj
      getString = fromString . getVal
      getStringArray = map fromString . fromArray . getVal
   in Machine
        { name = getString "name",
          alphabet = concat . getStringArray $ "alphabet",
          blank = head . getString $ "blank",
          states = getStringArray "states",
          initialState = getString "initial",
          finals = getStringArray "finals",
          transitions = createTransitions (getStringArray "states" \\ getStringArray "finals") (getVal "transitions"),
          currentState =
            State
              { state = fromString . getVal $ "initial",
                tape = (0, initialTape)
              }
        }

isValid :: Machine -> Either String ()
isValid m
  | nub m.alphabet /= m.alphabet = Left ("Alphabet contains duplicate characters (" ++ (show . nub $ (m.alphabet \\ nub m.alphabet)) ++ ")")
  | m.blank `notElem` m.alphabet = Left ("blank (" ++ [m.blank] ++ ") is not part of the alphabet (" ++ m.alphabet ++ ")")
  | not . null $ ((nub . snd) m.currentState.tape \\ m.alphabet) = Left ("Tape contains non-alphabet characters: (" ++ show ((nub . snd) m.currentState.tape \\ m.alphabet) ++ ")")
  | otherwise = Right ()

iterate :: Machine -> Either String Machine
iterate m =
  let newStates = concatMap (`transition` m.currentState) m.transitions
   in if length newStates /= 1
        then Left "No valid transitions!"
        else
          if (state . head $ newStates) `elem` m.finals
            then
              Left "final"
            else
              Right m {currentState = head newStates}
