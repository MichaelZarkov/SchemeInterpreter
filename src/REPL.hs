
module REPL (repl) where

import Interpret (parseAndInterpret)
import RacketEval (Env, RacketData (DNum, DCons, DLamb, DVoid, DEmptyList, DBool))
import System.IO (hFlush, stdout)

-- Read-evaluate-print-loop.
repl :: Env -> IO () 
repl en = do
  printAndFlush "> "
  codeToInterpret <- getUntil
  let (res, newEn, mErrMsg) = parseAndInterpret codeToInterpret en in do
    if null res
      then putStr ""
      else putStr $ foldr (\x r -> rctToString x ++ "\n" ++ r) [] res  -- Prints evaluation results.
    maybe (putStr "") putStrLn mErrMsg  -- Print errors if any.
    repl newEn  -- Loop.

-- Gets characters until '$' is seen.
getUntil :: IO String 
getUntil = do 
  x <- getChar
  if x == '$'
    then pure [] 
    else do
      xs <- getUntil 
      pure $ x:xs

rctToString :: RacketData -> String 
rctToString (DNum n) = show n
rctToString d@(DCons x y) =
  if isRctList y
    then "(list" ++ go d ++ ")"
    else "(cons " ++ rctToString x ++ " " ++ rctToString y ++ ")"
  where 
    go :: RacketData -> String 
    go (DCons a b) = " " ++ rctToString a  ++ go b
    go DEmptyList = "" 
    go _ = error "Logic error in 'rctToString' function!"
rctToString (DLamb _ _ _) = "#<procedure>"
rctToString DVoid = ""
rctToString DEmptyList = "(list)"
rctToString (DBool b)
  | b = "#t"
  | otherwise = "#f"

isRctList :: RacketData -> Bool 
isRctList (DCons _ y) = isRctList y
isRctList DEmptyList = True
isRctList _ = False  

printAndFlush :: String -> IO ()
printAndFlush s = do
  putStr s
  hFlush stdout