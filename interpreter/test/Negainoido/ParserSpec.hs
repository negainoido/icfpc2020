{-# LANGUAGE OverloadedStrings #-}
module Negainoido.ParserSpec where

import Test.Hspec
import Test.HUnit

import Negainoido.Syntax
import Negainoido.Parser
import Control.Monad.Except

spec :: Spec
spec = do
    describe "Negainoido.Parser.parse" $ do
        it "can parse ap ap cons t i" $ do
            let input = [Ap, Ap, Cons, T, I]
            let expected = App (HSymbol Cons) $ map symToExpr [I, T] 
            runExcept (parse input) `shouldBe` Right expected
        it "can parse ap ap cons t ap ap cons t nil" $ do
            let input = [Ap, Ap, Cons, T, Ap, Ap, Cons, T, Nil]
            let expected = App (HSymbol Cons) [
                    App (HSymbol Cons) [symToExpr Nil, symToExpr T],
                    symToExpr T
                    ] 
            runExcept (parse input) `shouldBe` Right expected
    describe "Negainoido.Parser.parseSymbol" $ do
        it "can parse add" $ do
            runExcept (parseSymbol "add") `shouldBe` Right Add
        it "can parse nonterminal" $ do
            runExcept (parseSymbol ":123") `shouldBe` Right (NonTerm (NT 123))
        it "can parse number" $ do
            runExcept (parseSymbol "123") `shouldBe` Right (Num 123)
    describe "Negainoido.Parser.parseDef" $ do
        it "can parse def with arity" $ do
            let input = ":1 3 = ap ap !2 !1 !0"
                expected = Def (NT 1) (App (HSymbol (Var (V 2))) [
                    symToExpr (Var (V 0)),
                    symToExpr (Var (V 1))
                  ]) 3
            runExcept (parseDef input) `shouldBe` Right expected