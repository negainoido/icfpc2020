{-# LANGUAGE OverloadedStrings #-}
module Negainoido.CompileSpec where

import Test.Hspec
import Test.HUnit

import qualified Data.Map as M
import Control.Monad.Except 
import Negainoido.Syntax
import Negainoido.Parser
import Negainoido.Compile

import Debug.Trace

expr x = e 
    where
    Right e = runExcept $ parseMain x 
def x = e
    where
    r = runExcept $ parseDef x
    e = case r of
        Left err -> error err
        Right v -> v

spec :: Spec
spec = do
    describe "Negainoido.Compile.simplify" $ do
        it "simplify S" $ do
            let input = expr "ap ap ap s add mul 2"
                expected = expr "ap ap add 2 ap mul 2"
            simplify input `shouldBe` expected
    describe "Negainoido.Compile.compileDef" $ do    
        it "compile :1143" $ do
            let input = def ":1143 = ap ap c ap ap c :1132 0 add"
                env = M.fromList [ (NT 1132, 0)]
                expected = (def ":1143 = ap ap ap :1132 !0 0 add") { defArity = 1}
            compileDef env input `shouldBe` expected
        it "compile s" $ do
            let input = def ":1 = s"
                env = M.fromList []
                expected = (def ":1 3 = ap ap !2 !0 ap !1 !0")
            compileDef env input `shouldBe` expected