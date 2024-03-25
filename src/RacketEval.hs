{-# LANGUAGE LambdaCase #-}

module RacketEval
(
  eval, isRctVar, toStr,
  BuiltInActions (Add, Sub, Mul, Div, Eql, Grt, Cns, Lst, Car, Cdr, IsNull, Def, Lam, If, Cond, Else), 
  Env,
  RacketData (DNum, DCons, DLamb, DVoid, DEmptyList, DBool)
) where 

import RacketParser ( RacketExp (Name, List) )
import Data.List ( find )
import Text.Read ( readMaybe )
import Data.Maybe ( isNothing )
import Text.Printf ( printf )


data RacketData =
  DNum Double  -- Should try to add int and bool type.
  | DCons RacketData RacketData
  -- Function definition. Note that functions with arbitrary number of arguments are not supported.
  -- A function has a list of parameters, a body and an environment in which it was evaluated.
  | DLamb [String] RacketExp Env
  -- Void type is returned in 'cond' statement when there is no else block and none of the conditions of all other blocks are true.
  -- Example: here void type is returned:
  --    (cond 
  --      ((= 0 1) 2)
  --      ((> 0 1) 5)
  --    )
  | DVoid
  | DEmptyList
  | DBool Bool  -- The constants '#t' and '#f' are not currently supported.
  deriving (Eq, Show)

type Env = [(String, RacketData)]

data BuiltInActions =
  Add
  | Sub
  | Mul
  | Div 
  | Eql
  | Grt
  | Cns 
  | Lst 
  | Car  
  | Cdr
  | IsNull
  | Def
  | Lam  
  | If
  | Cond  
  | Else
  deriving (Bounded, Enum)

-- Is this the best way to store reserved words?
toStr :: BuiltInActions -> String
toStr = \case
    Add -> "+"
    Sub -> "-"
    Mul -> "*"
    Div -> "/"
    Eql -> "="
    Grt -> ">"
    Cns -> "cons"
    Lst -> "list"
    Car -> "car"
    Cdr -> "cdr"
    IsNull -> "null?"
    Def -> "define"
    Lam -> "lambda"
    If  -> "if"
    Cond -> "cond"
    Else -> "else"

-- This is a bad way to do it but it will do for now. A better way would be to make a list of all the
-- constructors of 'BuiltInActions', convert them to strings and check the given string against them.
isReservedWord :: String -> Bool
isReservedWord s = s `elem` map toStr [minBound .. maxBound]

-- Note that here I'm not checking for brackets or white spaces in the potential name because that is
-- a job for the Parser.
isRctVar :: RacketExp -> Bool 
isRctVar (Name s) = not (isReservedWord s) && isNothing (readMaybe s :: Maybe Double)
isRctVar _ = False

areRctVars :: [RacketExp] -> Bool
areRctVars = all isRctVar

-- Functions with arbitrary number of arguments are not supported. Except for 'list'.  
-- But 'list' is special in the sense that this evaluator can't return it as a result from a procedure. 
-- So for example: 
--    (define f list)  -- this will give an error immediately.
--    (define f (lambda (x y z) list))  -- This will not give an error, because the body of the lambda is not evaluated.
--    (f 1 2 3)  -- But when trying to apply 'f', we'll get an error (because the body will be evaluated).
--
-- To do:
--  - explain 'Either'.
eval :: RacketExp -> Env -> Either String RacketData
eval (Name s) en
  | s == toStr Add = Right $ DLamb ["x","y"] (List [Name (toStr Add), Name "x", Name "y"]) []
  | s == toStr Sub = Right $ DLamb ["x","y"] (List [Name (toStr Sub), Name "x", Name "y"]) [] 
  | s == toStr Mul = Right $ DLamb ["x","y"] (List [Name (toStr Mul), Name "x", Name "y"]) [] 
  | s == toStr Div = Right $ DLamb ["x","y"] (List [Name (toStr Div), Name "x", Name "y"]) [] 
  | s == toStr Eql = Right $ DLamb ["x","y"] (List [Name (toStr Eql), Name "x", Name "y"]) []
  | s == toStr Grt = Right $ DLamb ["x","y"] (List [Name (toStr Grt), Name "x", Name "y"]) []
  | s == toStr Cns = Right $ DLamb ["x","y"] (List [Name (toStr Cns), Name "x", Name "y"]) [] 
  | s == toStr Lst = Left $ "Error! This interpreter doesn't support functions with arbitrary number of arguments, " ++
                            "so you can't return the procedure '" ++ toStr Lst ++ "' as a result!"
  | s == toStr Car = Right $ DLamb ["ordered-pair"] (List [Name (toStr Car), Name "ordered-pair"]) []
  | s == toStr Cdr = Right $ DLamb ["ordered-pair"] (List [Name (toStr Cdr), Name "ordered-pair"]) []
  | s == toStr IsNull = Right $ DLamb ["l"] (List [Name (toStr IsNull), Name "l"]) []
  | s == toStr Def = Left $ invalidUseOf $ toStr Def
  | s == toStr Lam = Left $ invalidUseOf $ toStr Lam
  | s == toStr If  = Left $ invalidUseOf $ toStr If
  | s == toStr Cond = Left $ invalidUseOf $ toStr Cond
  | s == toStr Else = Left $ invalidUseOf $ toStr Else
  -- If not a reserved word, check if it's a number or variable.
  | otherwise = 
    case readMaybe s of 
      Just x -> Right $ DNum x  -- 's' is a number.
      _ ->                      -- 's' is a variable.
        case find (\(str,_) -> s == str) en of
          Just (_,rctData) -> Right rctData
          _ -> Left $ printf "Error! Identifier '%s' is undefined!" s 
eval (List []) _ = Left "Error! '()' can be used only in lambda expressions!"
eval rctExp@(List (Name s:es)) en
  | s == toStr Add = 
    case es of 
      [arg1, arg2] -> 
        case (eval arg1 en, eval arg2 en) of 
          (Right (DNum x1), Right (DNum x2)) -> Right $ DNum $ x1+x2
          (Left errMsg,_) -> Left errMsg
          (_,Left errMsg) -> Left errMsg
          _ -> Left $ toStr Add `argsMustBe` "numbers"
      _ -> Left $ toStr Add `takesThisManyArgs` 2
  | s == toStr Sub =
    case es of 
      [arg1, arg2] -> 
        case (eval arg1 en, eval arg2 en) of 
          (Right (DNum x1), Right (DNum x2)) -> Right $ DNum $ x1-x2
          (Left errMsg,_) -> Left errMsg
          (_,Left errMsg) -> Left errMsg
          _ -> Left $ toStr Sub `argsMustBe` "numbers"
      _ -> Left $ toStr Sub `takesThisManyArgs` 2
  | s == toStr Mul =
    case es of 
      [arg1, arg2] -> 
        case (eval arg1 en, eval arg2 en) of 
          (Right (DNum x1), Right (DNum x2)) -> Right $ DNum $ x1*x2
          (Left errMsg,_) -> Left errMsg
          (_,Left errMsg) -> Left errMsg
          _ -> Left $ toStr Mul `argsMustBe` "numbers"
      _ -> Left $ toStr Mul `takesThisManyArgs` 2
  | s == toStr Div =
    case es of 
      [arg1, arg2] -> 
        case (eval arg1 en, eval arg2 en) of 
          (Right (DNum x1), Right (DNum x2)) -> Right $ DNum $ x1/x2
          (Left errMsg,_) -> Left errMsg
          (_,Left errMsg) -> Left errMsg
          _ -> Left $ toStr Div `argsMustBe` "numbers"
      _ -> Left $ toStr Div `takesThisManyArgs` 2
  | s == toStr Eql = 
    case es of
      [arg1, arg2] ->
        case (eval arg1 en, eval arg2 en) of
          (Right (DNum x), Right (DNum y)) -> Right $ DBool $ x == y
          (Left errMsg,_) -> Left errMsg
          (_,Left errMsg) -> Left errMsg
          _ -> Left $ toStr Eql `argsMustBe` "numbers"
      _ -> Left $ toStr Eql `takesThisManyArgs` 2
  | s == toStr Grt = 
    case es of
      [arg1, arg2] ->
        case (eval arg1 en, eval arg2 en) of
          (Right (DNum x), Right (DNum y)) -> Right $ DBool $ x > y
          (Left errMsg,_) -> Left errMsg
          (_,Left errMsg) -> Left errMsg
          _ -> Left $ toStr Grt `argsMustBe` "numbers"
      _ -> Left $ toStr Grt `takesThisManyArgs` 2
  | s == toStr Cns = 
    case es of 
      [arg1, arg2] -> 
        case (eval arg1 en, eval arg2 en) of 
          (Right r1, Right r2) -> Right $ DCons r1 r2
          (Left errMsg,_) -> Left errMsg
          (_,Left errMsg) -> Left errMsg
      _ -> Left $ toStr Cns `takesThisManyArgs` 2
  | s == toStr Lst =
    let evaluatedExps = map (`eval` en) es;
        isError :: Either String RacketData -> Bool;
        isError (Left _) = True;
        isError (Right _) = False in
      case find isError evaluatedExps of  -- Check if there's an error in evaluation.
        Just errMsg -> errMsg 
        _ -> Right $ foldr (DCons . (\(Right rctData) -> rctData)) DEmptyList evaluatedExps  -- A list is nested ordered pairs.
  | s == toStr Car = 
    case es of 
      [arg] -> 
        case eval arg en of 
          Right (DCons x _) -> Right x
          Left errMsg -> Left errMsg
          _ -> Left $ toStr Car `argsMustBe` "ordered pairs"
      _ -> Left $ toStr Car `takesThisManyArgs` 1
  | s == toStr Cdr =
    case es of 
      [arg] -> 
        case eval arg en of 
          Right (DCons _ y) -> Right y
          Left errMsg -> Left errMsg
          _ -> Left $ toStr Car `argsMustBe` "ordered pairs"
      _ -> Left $ toStr Car `takesThisManyArgs` 1
  | s == toStr IsNull = 
    case es of
      [arg] ->
        case eval arg en of
          Right DEmptyList -> Right $ DBool True
          Left errMsg -> Left errMsg
          _ -> Right $ DBool False
      _ -> Left $ toStr IsNull `takesThisManyArgs` 1
  | s == toStr Def = Left $ "Error! Can't use '" ++ toStr Def ++ "' in an expression!"
  | s == toStr Lam = 
    -- Here I'm allowing only expressions of the form: '(lambda <arguments> <body>)', but in Scheme
    -- we can have: '(lambda <arguments> <exp-1> ... <exp-n> <body>)'. Expressions 1 to n are evaluated
    -- and then 'body' is evaluated and the result from the body is returned.
    case es of 
      [Name arg, body] ->  -- One argument to the lambda function.
        if isRctVar (Name arg) 
          then Right $ DLamb [arg] body en
          else Left incorrectLambda
      [List args, body] ->  -- Zero or more arguments to the lambda function.
        if areRctVars args 
          then Right $ DLamb (map (\(Name var) -> var) args) body en
          else Left incorrectLambda
      _ -> Left $ toStr Lam `takesThisManyArgs` 2
  | s == toStr If  =
    case es of 
      [p, arg1, arg2] ->
        case eval p en of 
          Right (DBool True) -> eval arg1 en  -- In Scheme everything except 'true' is considered 'false'.
          Left errMsg -> Left errMsg 
          _ -> eval arg2 en
      _ -> Left $ toStr If `takesThisManyArgs` 3
  | s == toStr Cond = 
    -- This implementation of 'cond' is not exactly the same as in Scheme.
    case es of 
      [] -> Right DVoid  -- There's no else block in this 'cond' expression.
      (List [resExp] :_) -> eval resExp en  -- Block with missing condition is considered true.
      (List [Name "else",resExp] :_) -> eval resExp en  -- Reached the else block.
      (List [condition,resExp] : cases) -> 
        case eval condition en of 
          Right (DBool True) -> eval resExp en  -- In Scheme everything except 'true' is considered 'false'.
          Left errMsg -> Left errMsg 
          _ -> eval (List $ Name (toStr Cond):cases) en
      _ -> Left $ toStr Cond `argsMustBe` "lists with 1 or 2 elements"
  | s == toStr Else = Left $ invalidUseOf $ toStr Else
  | otherwise = evalLambdaApplication rctExp en  -- The given expression is a function application.
eval rctExp en = evalLambdaApplication rctExp en  -- The given expression is a function application.

evalLambdaApplication :: RacketExp -> Env -> Either String RacketData 
evalLambdaApplication (List (f:args)) en = 
  case eval f en of 
    Right (DLamb params body lambdaEnv) ->
      if length params /= length args 
        then Left "Error! Function application with incorrect number of arguments!"
        else 
          case bind (zip params args) en of 
            Right paramBindings -> eval body $ paramBindings ++ lambdaEnv ++ en  -- Notice the order of concatenation of the environments!
            Left errMsg -> Left errMsg
    Left errMsg -> Left errMsg
    _ -> Left $ isNotAProcedure $ show f
evalLambdaApplication _ _ = error "Error! 'evalLambdaApplication' expects a Racket expression which is a function application!"


-- Returns error message if error when evaluating the Racket expressions.
bind :: [(String, RacketExp)] -> Env -> Either String Env 
bind l en = 
  case find isError evaluated of 
    Just (_, Left errMsg) -> Left errMsg 
    _ -> Right $ map (\(s, Right rctData) -> (s,rctData)) evaluated 
  where 
    evaluated :: [(String, Either String RacketData)]
    evaluated = map (\(s,e) -> (s, eval e en)) l
    isError :: (String, Either String RacketData) -> Bool 
    isError (_, Left _) = True
    isError (_, Right _) = False


-- Functions for error messages.
invalidUseOf :: String -> String 
invalidUseOf s = "Error! Invalid use of '" ++ s ++ "'!"

takesThisManyArgs :: String -> Int -> String 
takesThisManyArgs s n = "Error! '" ++ s ++ "' takes " ++ show n ++ " arguments!" 

argsMustBe :: String -> String -> String 
argsMustBe f args = "Error! Arguments of '" ++ f ++ "' must be " ++ args ++ "!"

incorrectLambda :: String 
incorrectLambda = "Error! Incorrect lambda expression!"

isNotAProcedure :: String -> String 
isNotAProcedure x = "Error! '" ++ x ++ "' is not a procedure! Expected a procedure!"