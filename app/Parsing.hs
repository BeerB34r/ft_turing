{-# LANGUAGE LambdaCase #-}

module Parsing where

import Control.Applicative
import Data.Char

newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a)
  }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (input', x) <- p input
    Just (input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (Parser a) <*> (Parser b) = Parser $ \input -> do
    (input', f) <- a input
    (input'', x) <- b input'
    Just (input'', f x)

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser a) <|> (Parser b) = Parser $ \input -> a input <|> b input

-- lego bricks
parseOpt :: a -> Parser a -> Parser a
parseOpt x p = p <|> pure x

parseAny :: Parser Char
parseAny = Parser f
  where
    f (x : xs) = Just (xs, x)
    f [] = Nothing

parseChar :: Char -> Parser Char
parseChar c = Parser $ \case
  x : xs
    | x == c -> Just (xs, x)
    | otherwise -> Nothing
  _ -> Nothing

parseSet :: [Char] -> Parser Char
parseSet = foldr ((<|>) . parseChar) empty

parseStr :: String -> Parser String
parseStr = traverse parseChar

parsePred :: (Char -> Bool) -> Parser Char
parsePred p = Parser $ \case
  (x : xs) -> if p x then Just (xs, x) else Nothing
  [] -> Nothing

-- simple ones
ows :: Parser String
ows = many (parsePred isSpace)

rws :: Parser String
rws = some (parsePred isSpace)
