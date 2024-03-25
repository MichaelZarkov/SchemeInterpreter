{-# LANGUAGE ScopedTypeVariables #-}

module RacketParserSpec ( racketParserSpec ) where

import Test.Hspec ( describe, it, parallel, shouldBe, Spec )

import RacketParser ( parseExps, RacketExp (Name, List) )

racketParserSpec :: Spec
racketParserSpec = parallel $ do
  parseExpsSpec

parseExpsSpec :: Spec
parseExpsSpec = describe "parseExps" $ do
  describe "correct Scheme input" $ do
    it "works on the empty string" $ do
      parseExps [] `shouldBe` Just []
    it "works on an example with a single identifier" $ do
      parseExps "var23$%mmm82" `shouldBe` Just [Name "var23$%mmm82"]
    it "works on example-1 with a single list of identifiers" $ do
      parseExps "(   var1 mmm, +- 3.4 5)" `shouldBe` Just [List [Name "var1", Name "mmm,", Name "+-", Name "3.4", Name "5"]]
    it "works on example-2 with a single list of identifiers" $ do
      parseExps "  (\n\n\n var1 var-2 \t) \n" `shouldBe` Just [List [Name "var1", Name "var-2"]]
    it "works on example-3 with a single list of identifiers" $ do
      parseExps "()" `shouldBe` Just [List []]
    it "works on an example with a deep list" $ do
      parseExps "(2 (2 (2 ())) () (2 2) ((2 2 ())))" `shouldBe` Just [deepList]
    it "works on an example with multiple Scheme expressions" $ do 
      parseExps "## (exp1) (exp2) @@" `shouldBe` Just [Name "##", List [Name "exp1"], List [Name "exp2"], Name "@@"]

-- Corresponds to: (2 (2 (2 ())) () (2 2) ((2 2 ())))
deepList :: RacketExp
deepList = List [Name "2", List [Name "2", List [Name "2", List []]], List [], List [Name "2", Name "2"], List [List [Name "2", Name "2", List []]]]