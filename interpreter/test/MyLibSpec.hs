{-# LANGUAGE OverloadedStrings #-}
module MyLibSpec where

import Test.Hspec
import Test.HUnit

import MyLib

spec :: Spec
spec = do
    describe "MyLib.parse" $ do
        it "can parse ap ap cons t i" $ do
            let input = [Ap, Ap, Cons, T, I]
            let expected = App Cons $ map symToExpr [I, T] 
            parse input `shouldBe` expected
        it "can parse ap ap cons t ap ap cons t nil" $ do
            let input = [Ap, Ap, Cons, T, Ap, Ap, Cons, T, Nil]
            let expected = App Cons [
                    App Cons [symToExpr Nil, symToExpr T],
                    symToExpr T
                    ] 
            parse input `shouldBe` expected
    describe "MyLib.parseSymbol" $ do
        it "can parse add" $ do
            parseSymbol "add" `shouldBe` Add
        it "can parse nonterminal" $ do
            parseSymbol ":123" `shouldBe` NonTerm (NT 123)
        it "can parse number" $ do
            parseSymbol "123" `shouldBe` Num 123
    describe "MyLib.eval" $ do
        it "can eval statelessdraw" $ do
            let def = parseDef ":1 = ap ap c ap ap b b ap ap b ap b ap cons 0 ap ap c ap ap b b cons ap ap c cons nil ap ap c ap ap b cons ap ap c cons nil nil"
                main = parseMain "ap ap :1 nil ap ap cons 0 0"
                expected = Right (DCons (DNumber 0) (DCons DNil (DCons (DCons (DCons (DCons (DNumber 0) (DNumber 0)) DNil) DNil) DNil)))
            evalMain [def] main `shouldReturn` expected


                
    
