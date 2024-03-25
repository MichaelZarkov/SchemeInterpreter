{-# LANGUAGE ScopedTypeVariables #-}

module RacketEvalSpec ( racketEvalSpec ) where

import Data.Maybe ( fromJust )
import Test.Hspec ( describe, it, parallel, shouldBe, Spec )
import Test.Hspec.QuickCheck ( prop )

import RacketEval
  (
    eval, toStr,
    BuiltInActions (Add, Sub, Mul, Div, Eql, Grt, Cns, Lst, Car, Cdr, IsNull, Def, Lam, If, Cond, Else),
    RacketData (DNum, DCons, DLamb, DVoid, DEmptyList, DBool)
  )
import RacketParser ( parseExps, RacketExp (Name, List) )

racketEvalSpec :: Spec
racketEvalSpec = parallel $ do
  evalSpec

evalSpec :: Spec
evalSpec = describe "eval" $ do
  prop "evaluates a number to a number in empty environment" $ \(x :: Double) -> 
    eval (Name $ show x) [] `shouldBe` Right (DNum x)

  describe (toStr Add) $ do
    prop "correctly add two numbers" $ \(x :: Double) (y :: Double) ->
      eval (List [Name $ toStr Add, Name $ show x, Name $ show y]) [] `shouldBe` Right (DNum $ x+y)

  describe (toStr Sub) $ do
    prop "correctly subtracts two numbers" $ \(x :: Double) (y :: Double) ->
      eval (List [Name $ toStr Sub, Name $ show x, Name $ show y]) [] `shouldBe` Right (DNum $ x-y)
  
  describe (toStr Mul) $ do
    prop "correctly multiplies two numbers" $ \(x :: Double) (y :: Double) -> 
      eval (List [Name $ toStr Mul, Name $ show x, Name $ show y]) [] `shouldBe` Right (DNum $ x*y)
  
  describe (toStr Div) $ do 
    prop "correctly divides two numbers" $ \(x :: Double) (y :: Double) -> 
      eval (List [Name $ toStr Div, Name $ show x, Name $ show y]) [] `shouldBe` Right (DNum $ x/y)

  describe (toStr Eql) $ do
    prop "works correctly on numbers" $ \(x :: Double) (y :: Double) ->
      eval (List [Name $ toStr Eql, Name $ show x, Name $ show y]) [] `shouldBe` Right (DBool $ x==y)

  describe (toStr Grt) $ do 
    prop "works correctly on numbers" $ \(x :: Double) (y :: Double) ->
      eval (List [Name $ toStr Grt, Name $ show x, Name $ show y]) [] `shouldBe` Right (DBool $ x>y)

  describe (toStr Cns) $ do
    it "works on example-1" $ do 
      eval (parseExp' "(cons 1 2)") [] `shouldBe` Right (DCons (DNum 1) (DNum 2))
    it "works on example-2" $ do
      eval (parseExp' "(cons (cons x y) z)") [("x", DNum 1), ("y", DVoid), ("z", DBool True)]
      `shouldBe` Right (DCons (DCons (DNum 1) DVoid) (DBool True))

-- For convenience.
parseExp' :: String -> RacketExp
parseExp' = head . fromJust . parseExps