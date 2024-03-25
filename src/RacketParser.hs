
module RacketParser ( parseExps, RacketExp (Name, List) ) where

import Parser ( nom, parse, parseFailure, Parser )
import Control.Applicative ( some, many, (<|>) )
import Data.Char ( isSpace )

char :: Char -> Parser Char
char c = do
  x <- nom
  if x == c
    then pure x
    else parseFailure

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do 
  c <- nom 
  if p c then pure c else parseFailure

-- To do:
--  - explain the type ().
eatSpace :: Parser ()
eatSpace = do 
    many $ satisfy isSpace
    pure ()

data RacketExp = Name String | List [RacketExp] deriving (Eq, Show)

-- a Racket name is a string of characters that don't contain '(', ')' and white spaces.
nameParser :: Parser RacketExp 
nameParser = Name <$> some (satisfy isCharFromName) <* eatSpace 
  where 
    isCharFromName :: Char -> Bool 
    isCharFromName c = c /= '(' && c /= ')' && not (isSpace c)

listParser :: Parser RacketExp
listParser = do
  char '('
  eatSpace
  -- Empty list is considered valid. We can have for example "(lambda () 123)" which is a valid function in Racket.
  res <- many racketExpParser
  char ')'
  eatSpace
  pure $ List res 

-- Parses a single RacketExp.
-- To do:
--  - explain <|> and <*.
racketExpParser :: Parser RacketExp
racketExpParser = (nameParser <|> listParser) <* eatSpace

racketExpsParser :: Parser [RacketExp]
racketExpsParser = do 
  eatSpace 
  many racketExpParser

parseExps :: String -> Maybe [RacketExp] 
parseExps = parse racketExpsParser
