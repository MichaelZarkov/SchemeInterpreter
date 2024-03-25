
module Interpret (parseAndInterpret) where 

import RacketEval (BuiltInActions (Def), Env, RacketData, eval, isRctVar, toStr)
import RacketParser (RacketExp (Name, List), parseExps)

-- Interprets the given Racket expressions in the given environment.
-- Returns the results of the evaluation of the expressions, 
-- returns the environment with the new bindings made to it, 
-- returns if an error has occurred.
interpret :: [RacketExp] -> Env -> ([RacketData], Env, Maybe String)
interpret [] en = ([], en, Nothing)   
interpret l@(List [Name d, Name s, expr] : es) en
  | d == toStr Def = 
    if isRctVar $ Name s  -- Check if 's' is a valid Racket name.
      then 
          case eval expr en of 
            Right exp' -> interpret es $ (s,exp'):en
            Left errMsg -> ([], en, Just errMsg)
      else ([], en, Just $ "Error! Name of the binding '" ++ s ++ "' in '" ++ toStr Def ++ "' must be a valid Racket name!")
  | otherwise = interpFstExp l en
interpret es en = interpFstExp es en

-- The first expression is expected to not be a definition.
interpFstExp :: [RacketExp] -> Env -> ([RacketData], Env, Maybe String)
interpFstExp [] en = ([], en, Nothing)
interpFstExp (e:es) en =
  let (es', en', err') = interpret es en in 
    case eval e en of 
      Right e' -> (e':es', en', err')
      Left errMsg -> ([], en, Just errMsg)

parseAndInterpret :: String -> Env -> ([RacketData], Env, Maybe String)
parseAndInterpret str en = 
  case parseExps str of 
    Just es -> interpret es en
    _ -> ([], en, Just "Parse error!")
