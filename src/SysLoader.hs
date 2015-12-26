module SysLoader(
  System(..),
  Object(..),
  loadSystem) where

import Data.Either

import Text.Parsec

import Data.Either

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number


data System = System {
    name        :: String,
    description :: String,
    objects     :: [Object]
  } deriving (Show)
data Object = Sun String Float Float Float | Planet String Float Float Float deriving Show

systemParser :: Parser System
systemParser = System <$> stringFieldParse "name" <*> stringFieldParse "description" <*> objectsFieldParse "objects"

eol =   Text.Parsec.try (string "\n\r")
    <|> Text.Parsec.try (string "\r\n")
    <|> string "\n"
    <|> string "\r"


parseObject :: Parser Object
parseObject = do
  spaces
  skipMany eol
  typ <- string "Sun" <|> string "Planet"
  spaces
  name <- quotedString
  spaces
  char '('
  spaces
  string "V3"
  spaces
  x <- floating'
  spaces
  y <- floating'
  spaces
  z <- floating'
  spaces
  char ')'
  skipMany eol >> spaces
  char ',' <|> char ']'
  skipMany eol >> spaces
  case typ of
    "Sun"    -> return $ Sun name x y z
    "Planet" -> return $ Planet name x y z
    _        -> error "impossible!"

floating' = do
  sign <- optionMaybe $ char '-'
  num <- floating
  case sign of
    Nothing -> return $ num
    _       -> return $ -num

objectsFieldParse :: String -> Parser [Object]
objectsFieldParse str = do
  skipMany commentParse <|> skipMany eol
  string str
  spaces
  char '='
  spaces
  char '['
  objs <- many $ parseObject
  spaces
  skipMany eol
  return objs

stringFieldParse :: String -> Parser String
stringFieldParse str = do
  skipMany commentParse <|> skipMany eol
  string str
  spaces
  char '='
  spaces
  rest <- quotedString
  skipMany eol
  return rest

quotedString = do
  char '"'
  x <- many (noneOf "\"" <|> (char '\\' >> char '\"'))
  char '"'
  return x

commentParse :: Parser ()
commentParse = do
  string "--"
  skipMany $ noneOf "\n"
  eol
  return ()

loadSystem :: FilePath -> IO System
loadSystem path = parseSystem <$> readFile path

parseSystem :: String -> System
parseSystem = f . parse systemParser ""

f (Left x) = error $ show x
f (Right x) = x

