{-# LANGUAGE InstanceSigs #-}

module Parser
  ( Parser,
    parseMany,
    parse,
    nom,
    parseFailure,
  )
where

import Control.Applicative
  ( Alternative (empty, (<|>)),
    Applicative (liftA2),
  )

-- agenda:
-- 0. reminder of what we did last time - look at the combinators and show them briefly again
-- 1. newtype -- ask if fine with records?
-- 2. lettertype example + optional as example motivators for fmap? -- add Functor
-- 3. add Functor, add Applicative
-- 4. add Monad, show do desugaring
-- 5. mention how we get some free stuff - many and some are important, when, guard, sequence, traverse?
-- 6. write simple stack calculator parser to show how we use a parser
--    add
--    mult
--    incr
--    push
-- 7. json?

newtype Parser a = MkParser {runParser :: String -> [(String, a)]}

parseMany :: Parser a -> String -> [(String, a)]
parseMany = runParser

parse :: Parser a -> String -> Maybe a
parse px str =
  case runParser px str of
    [] -> Nothing
    (_, x) : _ -> Just x

nom :: Parser Char
nom =
  MkParser $ \str ->
    case str of
      [] -> []
      c : rest -> [(rest, c)]

(>>>=) :: Parser a -> (a -> Parser b) -> Parser b
px >>>= f =
  MkParser $ \str ->
    [ (rest2, y)
      | (rest1, x) <- runParser px str,
        (rest2, y) <- runParser (f x) rest1
    ]

mapParser :: (t -> a) -> Parser t -> Parser a
mapParser f px =
  MkParser $ \str ->
    map (\(s, a) -> (s, f a)) $ runParser px str

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap = mapParser

succeed :: a -> Parser a
succeed x =
  MkParser $ \str -> [(str, x)]

parse2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parse2 f px py =
  MkParser $ \str ->
    [ (rest2, f x y)
      | (rest1, x) <- runParser px str,
        (rest2, y) <- runParser py rest1
    ]

instance Applicative Parser where
  pure :: a -> Parser a
  pure = succeed

  liftA2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
  liftA2 = parse2

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) = (>>>=)

parseFailure :: Parser a
parseFailure = MkParser $ \_ -> []

instance Alternative Parser where
  empty :: Parser a
  empty = parseFailure

  (<|>) :: Parser a -> Parser a -> Parser a
  px <|> py =
    MkParser $ \str -> runParser px str ++ runParser py str
