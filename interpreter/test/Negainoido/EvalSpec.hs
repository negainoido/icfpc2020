{-# LANGUAGE OverloadedStrings #-}
module Negainoido.EvalSpec where

import Test.Hspec
import Test.HUnit

import MyLib
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