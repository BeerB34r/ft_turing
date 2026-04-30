module Json where

import Control.Applicative
import Data.Char
import Data.Functor
import Parsing

data JsonValue
  = Number Float
  | String String
  | Bool Bool
  | Array [JsonValue]
  | Object [(JsonValue, JsonValue)]
  | Null

-- prettier printing

instance Show JsonValue where
  show (Number f) = show f
  show (String s) = show s
  show (Bool b) = show b
  show (Array x) = "Array: [\n" ++ (unlines . map show) x ++ "]\n"
  show (Object x) = "Object {\n" ++ (unlines . map (\(a, b) -> show a ++ ":" ++ show b)) x ++ "}\n"
  show Null = "Null"

-- Json parsing business logic

jsonNumber :: Parser JsonValue
jsonNumber =
  let float =
        let odigits = many $ parsePred isDigit
            rdigits = some $ parsePred isDigit
            dot = parseChar '.'
         in ((++) <$> rdigits <*> parseOpt "" ((:) <$> dot <*> odigits))
              <|> ("0" ++) <$> ((:) <$> dot <*> rdigits)
   in Number . read <$> float

jsonBool :: Parser JsonValue
jsonBool =
  let jsonTrue = parseStr "true" $> True
      jsonFalse = parseStr "false" $> False
   in Bool <$> (jsonTrue <|> jsonFalse)

jsonString :: Parser JsonValue
jsonString =
  let string = many $ parsePred (/= '"')
   in String <$> (parseChar '"' *> string <* parseChar '"')

jsonNull :: Parser JsonValue
jsonNull = parseStr "null" $> Null

jsonArray :: Parser JsonValue
jsonArray =
  let values = many (jsonValue <* ows <* parseChar ',' <* ows)
      open = parseChar '[' <* ows
      close = ows *> parseChar ']'
      snoc x y = x ++ [y]
   in Array <$> (open *> parseOpt [] (snoc <$> values <*> jsonValue) <* close)

jsonObject :: Parser JsonValue
jsonObject =
  let key = jsonString <* ows <* parseChar ':'
      value = ows *> jsonValue <* ows
      elements = ((,) <$> key <*> (value <* parseChar ',' <* ows))
      element = ((,) <$> key <*> value)
      open = parseChar '{' <* ows
      close = parseChar '}'
      snoc x y = x ++ [y]
   in Object <$> (open *> parseOpt [] (snoc <$> many elements <*> element) <* close)

jsonValue :: Parser JsonValue
jsonValue =
  jsonNumber
    <|> jsonString
    <|> jsonBool
    <|> jsonArray
    <|> jsonObject
    <|> jsonNull
