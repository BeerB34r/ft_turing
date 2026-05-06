{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Turing where

import Control.Applicative
import Data.Bifunctor
import Data.Char
import Data.Either
import Data.List
import Json

capitalise :: String -> String
capitalise (x : xs) =
  let recurse (a, b) = map toLower a ++ capitalise b
      splitSpaces = (\(a, (b, c)) -> (a ++ b, c)) . second (span isSpace) . break isSpace
   in toUpper x : (recurse . splitSpaces) xs
capitalise _ = []

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
        showCurrentState = "\tCurrent state: " ++ show m.currentState
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

createTransition :: String -> Char -> String -> Char -> String -> Char -> Transition
createTransition initState c nextState w action b =
  let sameState = (== initState) . state
      sameChar = (== c) . current
      isTransitionValid s = sameState s && sameChar s
      moveHead i = i + (if action == "LEFT" then -1 else 1)
      modifyTape s i = take i s ++ [w] ++ drop (i + 1) s
      doTransition s =
        [ State
            { state = nextState,
              tape = (moveHead . fst . tape $ s, uncurry (flip modifyTape) . tape $ s)
            } -- we're on the tape somewhere
          | (moveHead . fst $ s.tape) < (length . snd $ s.tape)
              && (moveHead . fst $ s.tape) >= 0
        ]
          <|> [ State
                  { state = nextState,
                    tape = (moveHead . fst $ s.tape, uncurry (flip modifyTape) s.tape ++ [b])
                  } -- we walked off the right end of the tape
                | (moveHead . fst $ s.tape) >= 0
              ]
          <|> [ State
                  { state = nextState,
                    tape = (0, b : uncurry (flip modifyTape) s.tape)
                  } -- we walked off the left end, _always_ available
              ]
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

createTransitions :: [String] -> JsonValue -> Char -> [Transition]
createTransitions (x : xs) (Object obj) b =
  let currentTransitions = concatMap (fromArray . snd) . filter ((== x) . fromString . fst) $ obj -- [jsonObject]
      createSingle o =
        createTransition
          x
          (head . fromString . head . getValue o $ "read")
          (fromString . head . getValue o $ "to_state")
          (head . fromString . head . getValue o $ "write")
          (fromString . head . getValue o $ "action")
          b
   in map createSingle currentTransitions ++ createTransitions xs (Object obj) b
createTransitions _ _ _ = []

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
          transitions = createTransitions (getStringArray "states" \\ getStringArray "finals") (getVal "transitions") (head . getString $ "blank"),
          currentState =
            State
              { state = fromString . getVal $ "initial",
                tape = (0, initialTape)
              }
        }

isJsonValid :: JsonValue -> Either String ()
isJsonValid =
  let fieldIsString o s = case getValue (Object o) s of
        [] -> Left (capitalise s ++ " must exist")
        [String value]
          | not . null $ value -> Right ()
          | otherwise -> Left (capitalise s ++ " must be non-empty")
        [_] -> Left (capitalise s ++ " must be a string")
        _ -> Left (capitalise s ++ " cannot have duplicates")
      fieldIsArray o s = case getValue (Object o) s of
        [] -> Left (capitalise s ++ " must exist")
        [Array value]
          | not . null $ value -> Right ()
          | otherwise -> Left (capitalise s ++ " must be non-empty")
        [_] -> Left (capitalise s ++ " must be an array")
        _ -> Left (capitalise s ++ " cannot have duplicates")
      fieldIsStringArray o s
        | isLeft . fieldIsArray o $ s = fieldIsArray o s
        | (any (null . fromString) . fromArray . head . getValue (Object o)) s = Left (capitalise s ++ " cannot have empty members")
        | otherwise = Right ()
      checkAlphabet o
        | isLeft . fieldIsArray o $ "alphabet" = fieldIsArray o "alphabet"
        | (any ((/= 1) . length . fromString) . fromArray . head . getValue (Object o)) "alphabet" = Left ("Alphabet must have exclusively 1-length members (" ++ show (filter ((/= 1) . length . fromString) . fromArray . head . getValue (Object o) $ "alphabet") ++ ")")
        | otherwise = Right ()
      unknownTransitions o value = (map (fromString . fst) value \\ (map fromString . fromArray . head . getValue (Object o) $ "states"))
      getInnerValues s = map (head . (`getValue` s)) . fromArray
      checkDupReads transes = getInnerValues "read" transes /= (nub . getInnerValues "read") transes
      checkInvalidRW s o transes = (nub . map fromString . getInnerValues s) transes \\ (map fromString . fromArray . head . getValue (Object o) $ "alphabet")
      checkInvalidNext o transes = (nub . map fromString . getInnerValues "to_state") transes \\ (map fromString . fromArray . head . getValue (Object o) $ "states")
      checkInvalidAction transes = (nub . map fromString . getInnerValues "action") transes \\ ["RIGHT", "LEFT"]
      bun a = nub (a \\ nub a)
      checkTransitions o = case getValue (Object o) "transitions" of
        [] -> Left "Transitions must exist"
        [Object value]
          | (nub . map fst) value /= map fst value -> Left ("Transitions cannot have duplicates (" ++ show (map fst value \\ (nub . map fst) value) ++ ")")
          | any (checkDupReads . snd) value -> Left ("Transitions cannot have duplicate reads (" ++ show (concatMap bun (map (getInnerValues "read" . snd) value \\ map (nub . getInnerValues "read" . snd) value)) ++ ")")
          | not (all (null . checkInvalidRW "read" o . snd) value) -> Left ("Unknown read in transition (" ++ show (concatMap (nub . checkInvalidRW "read" o . snd) value) ++ ")")
          | not (all (null . checkInvalidRW "write" o . snd) value) -> Left ("Unknown write in transition (" ++ show (concatMap (nub . checkInvalidRW "write" o . snd) value) ++ ")")
          | not (all (null . checkInvalidNext o . snd) value) -> Left ("Unknown state in transition (" ++ show (concatMap (nub . checkInvalidNext o . snd) value) ++ ")")
          | not (all (null . checkInvalidAction . snd) value) -> Left ("Unknown action in transition (" ++ show (concatMap (nub . checkInvalidAction . snd) value) ++ ")")
          | not . null $ unknownTransitions o value -> Left ("Transitions cannot happen from unknown states (" ++ show (unknownTransitions o value) ++ ")")
          | null value -> Left "Transitions must be non-empty"
          | otherwise -> Right ()
        [_] -> Left "Transitions must be an object"
        _ -> Left "Transitions cannot have duplicates"
   in \case
        (Object o)
          -- duplicates
          | (nub . map fst) o /= map fst o -> Left ("Fields cannot have duplicates (" ++ show (bun $ map fst o) ++ ")")
          -- required fields
          | isLeft . fieldIsString o $ "name" -> fieldIsString o "name"
          | isLeft . checkAlphabet $ o -> checkAlphabet o
          | isLeft . fieldIsString o $ "blank" -> fieldIsString o "blank"
          | (/= 1) . length . fromString . head . getValue (Object o) $ "blank" -> Left ("Blank must be a single character (" ++ (show . head . getValue (Object o) $ "blank") ++ ")")
          | isLeft . fieldIsStringArray o $ "states" -> fieldIsStringArray o "states"
          | isLeft . fieldIsString o $ "initial" -> fieldIsString o "initial"
          | isLeft . fieldIsStringArray o $ "finals" -> fieldIsStringArray o "finals"
          | isLeft . checkTransitions $ o -> checkTransitions o
          | otherwise -> Right ()
        _ -> Left "Turing machine must be provided as single JSON object"

isValid :: Machine -> Either String ()
isValid m
  -- duplicates
  | nub m.alphabet /= m.alphabet = Left ("Alphabet contains duplicates (" ++ (show . nub $ (m.alphabet \\ nub m.alphabet)) ++ ")")
  | nub m.states /= m.states = Left ("States contains duplicates (" ++ (show . nub $ (m.states \\ nub m.states)) ++ ")")
  | nub m.finals /= m.finals = Left ("Finals contains duplicates (" ++ (show . nub $ (m.finals \\ nub m.finals)) ++ ")")
  -- subsets
  | m.blank `notElem` m.alphabet = Left ("Blank (" ++ [m.blank] ++ ") is not part of the alphabet (" ++ show m.alphabet ++ ")")
  | m.blank `elem` snd m.currentState.tape = Left "Blank can not be part of initial state"
  | not . null $ ((nub . snd) m.currentState.tape \\ m.alphabet) = Left ("Tape contains non-alphabet characters: (" ++ show ((nub . snd) m.currentState.tape \\ m.alphabet) ++ ")")
  | m.initialState `notElem` m.states = Left ("Initial state (" ++ show m.initialState ++ ") unkown")
  | not . null $ (m.finals \\ m.states) = Left ("Finals contains unknown state (" ++ show (m.finals \\ m.states) ++ ")")
  | otherwise = Right ()

iterate :: Machine -> Either String Machine
iterate m =
  let newStates = concatMap (`transition` m.currentState) m.transitions
   in if null newStates
        then Left "No valid transitions!"
        else
          if (state . head $ newStates) `elem` m.finals
            then
              Left "final"
            else
              Right m {currentState = head newStates}
