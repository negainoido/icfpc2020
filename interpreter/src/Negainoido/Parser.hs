{-# LANGUAGE OverloadedStrings #-}
module Negainoido.Parser where

import qualified Data.Text as T
import Data.Text(Text)
import Text.Read
import Control.Monad.Except
--import Debug.Trace
import Negainoido.Syntax

parseDef :: Text  -> Except String Def
parseDef txt = do
    (hd, body, arity) <- case T.words txt of
        hd: "=": body -> pure (hd, body, 0)
        hd: arity: "=" : body -> pure (hd, body, read (T.unpack arity))
        _ -> throwError $ "failed to parse definiton: " ++ T.unpack txt
    Def <$> (parseHead hd) <*> (parseBody body) <*> pure arity
    
parseHead :: Text -> Except String NT
parseHead txt =
    pure $ NT (read (T.unpack (T.tail txt)))

parseVar :: Text -> Except String V
parseVar txt =
    pure $ V (read (T.unpack (T.tail txt)))

parseMain :: Text -> Except String Expr
parseMain = parseBody . T.words

parseBody :: [Text] -> Except String Expr
parseBody = mapM parseSymbol >=> parse

parseSymbol :: Text -> Except String Symbol
parseSymbol x = 
    case x of
        "add" -> pure  Add
        "ap" -> pure Ap
        "b" -> pure B
        "c" -> pure C
        "car" -> pure Car
        "cdr" -> pure Cdr
        "cons" -> pure Cons
        "div" -> pure Div
        "eq" -> pure Eq
        "i" -> pure I
        "isnil" -> pure IsNil
        "lt" -> pure Lt
        "mul" -> pure Mul
        "neg" -> pure Neg
        "nil" -> pure Nil
        "s" -> pure S
        "t" -> pure T
        "f" -> pure F
        _ | T.head x == ':' -> NonTerm <$> (parseHead x)
          | T.head x == '!' -> Var <$> (parseVar x)
          | Just n <- readMaybe (T.unpack x) -> pure $ Num n
        _ -> throwError $ "unknown symbol:" ++ show x

data StackElem = SExpr Expr | SAp 
    deriving(Show)

parse :: [Symbol] -> Except String Expr
parse = go []
  where
      go :: [StackElem] -> [Symbol] -> Except String Expr
      go (SExpr c1: SExpr c2: SAp: st) syms = 
          go (SExpr (app c2 c1):st) syms
      go st (c:syms) = go (toStackElem c: st) syms
      go [SExpr e] [] = pure e
      go st tokens = throwError $ "Unexpected parse state: Stack = " ++ show st ++ " Token = " ++ show (take 3 tokens)
      toStackElem Ap = SAp
      toStackElem c = SExpr (symToExpr c)