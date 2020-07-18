{-# LANGUAGE OverloadedStrings #-}
module MyLibSpec where

import Test.Hspec
import Test.HUnit

import MyLib
import Negainoido.Syntax
import Negainoido.Parser
import Control.Monad.Except

spec :: Spec
spec = do
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


                
    
