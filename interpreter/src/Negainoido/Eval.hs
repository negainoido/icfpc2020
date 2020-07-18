{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE DeriveGeneric #-} 
{-# LANGUAGE RecordWildCards #-} 
module Negainoido.Eval where

import qualified Data.Map as M
import Control.Monad.Fix
import Data.IORef
import Control.Monad.Except
--import Debug.Trace
import Negainoido.Syntax

type Env = M.Map NT Thunk

evalForce :: Value -> ExceptT String IO SData
evalForce (VNumber v) = pure $ DNumber v
evalForce (VPApp Nil []) = pure $ DNil
evalForce (VPApp Cons [t2, t1]) = DCons <$> (evalThunk t1 >>= evalForce) <*> (evalThunk t2 >>= evalForce)
evalForce e = throwError $ "cannot force partial application" ++ show e

mkEnv :: [Def] -> ExceptT String IO Env
mkEnv defs = 
    mfix $ \env -> 
        M.fromList <$> (
            forM defs $ \(Def hd body) -> do
                thunk <- mkThunk body (eval env body)
                pure (hd, thunk))

evalMain :: [Def] -> Expr -> ExceptT String IO SData
evalMain defs expr = do
    env <- mkEnv defs 
    eval env expr >>= evalForce

eval :: Env -> Expr -> ExceptT String IO Value
--eval env e | traceShow ("eval", e) False = undefined
eval env (EThunk t args) = do
    v <- evalThunk t
    case (v, args) of
        (VPApp hd args', _args) -> 
            let toExpr x = EThunk x [] in
            eval env (App hd (args ++ (map toExpr args')))
        (_, []) -> pure v
        (_, _) -> throwError $ "cannot apply: " ++ show (v, args)
eval env (App hd args) = 
    let triOp f
            | e0:e1:e2:es <- reverse args = Just $ foldl app (f e0 e1 e2) es  
            | otherwise = Nothing
        triOpM f
            | e0:e1:e2:es <- reverse args = Just $ do
                e' <- f e0 e1 e2
                pure $ foldl app e' es  
            | otherwise = Nothing 
        uniOp f 
            | e0: es <- reverse args = Just $ foldl app (f e0) es
            | otherwise = Nothing
        uniOpM f
            | e0: es <- reverse args = Just $ do
                e' <- f e0
                pure $ foldl app e' es
            | otherwise = Nothing
        binOp f 
            | e0:e1: es <- reverse args = Just $ foldl app (f e0 e1) es
            | otherwise = Nothing
        binOpM f
            | e0:e1: es <- reverse args = Just $ do
                 e' <- f e0 e1
                 pure $ foldl app e' es
            | otherwise = Nothing
            in
    case (hd, args) of
        (Add, [e2, e1]) -> do 
            n1 <- eval env e1 >>= ensureNumber
            n2 <- eval env e2 >>= ensureNumber
            pure $ VNumber $ n1 + n2
        (Mul, [e2, e1]) -> do 
            n1 <- eval env e1 >>= ensureNumber
            n2 <- eval env e2 >>= ensureNumber
            pure $ VNumber $ n1 * n2
        (Div, [e2, e1]) -> do 
            n1 <- eval env e1 >>= ensureNumber
            n2 <- eval env e2 >>= ensureNumber
            pure $ VNumber $ n1 `div` n2
        (Neg, [e]) -> do
            n <- eval env e >>= ensureNumber
            pure $ VNumber $ negate n
        (B, _) | Just e <- triOp f -> eval env e
            where f e0 e1 e2 = app e0 (app e1 e2)
        (C, _) | Just e <- triOp f -> eval env e
            where f e0 e1 e2 = app (app e0 e2) e1
        (S, _) | Just me <- triOpM f -> me >>= eval env
            where
            f e0 e1 e2 =  do
                t2 <- mkThunk e2 (eval env e2)
                let e2' = EThunk t2 []
                pure $ app (app e0 e2') (app e1 e2')
        (I, _) | Just e <- uniOp id ->  eval env e
        (T, _) | Just e <- binOp (\x _ -> x) -> eval env e
        (F, _) | Just e <- binOp (\_ y -> y) -> eval env e
        (Car, _) | Just e <- uniOp f ->  eval env e
            where
            f e = app e (symToExpr T)
        (Cdr, _) | Just e <- uniOp f ->  eval env e
            where
            f e = app e (symToExpr F)
        (Nil, _) | Just e <- uniOp f -> eval env e
            where
            f _ = symToExpr T
        (Cons, _) | Just e <- triOp f -> eval env e
            where
            f e0 e1 e2 = app (app e2 e0) e1
        (IsNil, _) | Just me <- uniOpM f -> me >>= eval env
            where
            f e = do
                v <- eval env e
                case v of
                    VPApp Nil _ -> pure $ symToExpr T
                    VPApp Cons _ -> pure $ symToExpr F
                    _ -> throwError $ "Nil or Cons is expected but found" ++ show v
        (Lt, _) | Just me <- binOpM f -> me >>= eval env
            where
                f e1 e2 = do
                    n1 <- eval env e1 >>= ensureNumber
                    n2 <- eval env e2 >>= ensureNumber
                    pure $ if n1 < n2
                        then symToExpr T
                        else symToExpr F
        (Eq, _) | Just me <- binOpM f -> me >>= eval env
            where
                f e1 e2 = do
                    n1 <- eval env e1 >>= ensureNumber
                    n2 <- eval env e2 >>= ensureNumber
                    pure $ if n1 == n2
                        then symToExpr T
                        else symToExpr F
        (NonTerm n, es) -> do -- es [ e_n, ... , e2, e1, e0]
            t <- case M.lookup n env of
                Nothing -> throwError $  "Undefined Nonterminal: " ++ show n
                Just v -> pure v
            eval env (EThunk t es)
        (Num n, []) -> pure $ VNumber n
        _ -> 
            VPApp hd <$> forM args (\e -> mkThunk e (eval env e)) 

mkThunk :: Expr -> ExceptT String IO Value -> ExceptT String IO Thunk
mkThunk e action = liftIO $ Thunk e <$> newIORef (Left action)

evalThunk :: Thunk -> ExceptT String IO Value
evalThunk (Thunk _ ref) = do
    r <- liftIO $ readIORef ref
    case r of
        Right v -> pure v
        Left action -> do
            v <- action
            liftIO $ writeIORef ref (Right v)
            pure v

ensureNumber :: Value -> ExceptT String IO Integer
ensureNumber (VNumber x) = pure x
ensureNumber e = throwError $ "Number is expected but found: " ++ show e 
