{-# LANGUAGE OverloadedStrings #-}
module Negainoido.EvalSpec where

import Test.Hspec
import Test.HUnit

import Negainoido.Syntax
import Negainoido.Parser
import Negainoido.Eval
import Control.Monad.Except

spec :: Spec
spec = do
    describe "Eval.evalMain" $ do
        it "can eval statelessdraw" $ do
            let Right def = runExcept $ parseDef ":1 = ap ap c ap ap b b ap ap b ap b ap cons 0 ap ap c ap ap b b cons ap ap c cons nil ap ap c ap ap b cons ap ap c cons nil nil"
                Right main = runExcept $ parseMain "ap ap :1 nil ap ap cons 0 0"
                expected = Right $
                    DCons (DNumber 0) $
                    DCons DNil $ 
                        DCons (DCons (DCons (DCons (DNumber 0) (DNumber 0)) DNil) DNil) DNil
            runExceptT (evalMain [def] main) `shouldReturn` expected
        it "can eval variable" $ do
            let Right def = runExcept $ parseDef ":1 3 = ap ap mul !1 ap ap add !0 !2"
                Right main = runExcept $ parseMain "ap ap ap :1 1 2 3"
                expected = Right (DNumber 8)
            runExceptT (evalMain [def] main) `shouldReturn` expected
        it "can eval variable" $ do
            let Right def = runExcept $ parseDef ":1 2 = ap ap cons !1 !0"
                Right main = runExcept $ parseMain "ap ap :1 1 2"
                expected = Right (DCons (DNumber 1) (DNumber 2))
            runExceptT (evalMain [def] main) `shouldReturn` expected
            