{-# LANGUAGE OverloadedStrings #-}
module MyLibSpec where

import Test.Hspec
import Test.HUnit

import MyLib
import Control.Monad.Except

spec :: Spec
spec = do
    describe "MyLib.parse" $ do
        it "can parse ap ap cons t i" $ do
            let input = [Ap, Ap, Cons, T, I]
            let expected = App Cons $ map symToExpr [I, T] 
            runExcept (parse input) `shouldBe` Right expected
        it "can parse ap ap cons t ap ap cons t nil" $ do
            let input = [Ap, Ap, Cons, T, Ap, Ap, Cons, T, Nil]
            let expected = App Cons [
                    App Cons [symToExpr Nil, symToExpr T],
                    symToExpr T
                    ] 
            runExcept (parse input) `shouldBe` Right expected
    describe "MyLib.parseSymbol" $ do
        it "can parse add" $ do
            runExcept (parseSymbol "add") `shouldBe` Right Add
        it "can parse nonterminal" $ do
            runExcept (parseSymbol ":123") `shouldBe` Right (NonTerm (NT 123))
        it "can parse number" $ do
            runExcept (parseSymbol "123") `shouldBe` Right (Num 123)
    describe "MyLib.eval" $ do
        it "can eval statelessdraw" $ do
            let Right def = runExcept $ parseDef ":1 = ap ap c ap ap b b ap ap b ap b ap cons 0 ap ap c ap ap b b cons ap ap c cons nil ap ap c ap ap b cons ap ap c cons nil nil"
                Right main = runExcept $ parseMain "ap ap :1 nil ap ap cons 0 0"
                expected = Right Result {
                    returnValue = 0,
                    stateData = DNil,
                    imageList = Just [[(0,0)]],
                    imageListAsData = (DCons (DCons (DCons (DNumber 0) (DNumber 0)) DNil) DNil)
                }
            evalMain [def] main `shouldReturn` expected


                
    
