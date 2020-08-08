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
import qualified Data.Sequence as Q

type Env = M.Map NT Thunk

evalForce :: Value -> ExceptT String IO SData
evalForce (VNumber v) = pure $ DNumber v
evalForce (VNil ) = pure $ DNil
evalForce (VCons t1 t2) = 
    DCons <$> (pure t1 >>= evalForce) <*> (pure t2 >>= evalForce)
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
    let toExpr x = EThunk x Q.Empty
    case (v, args) of
        (VPApp hd args', _args) -> 
            eval env (App hd ((fmap toExpr args') <> args))
        (_, Q.Empty) -> pure v
        (VNil, _ Q.:<| es) ->
            eval env (App T es)
        (VCons v1 v2, p Q.:<| es) -> do
            t1 <- mkThunk undefined (pure v1)
            t2 <- mkThunk undefined (pure v2)
            eval env (appArgs p (toExpr t1 Q.:<| toExpr t2 Q.:<| es))
        (_, _) -> throwError $ "cannot apply: " ++ show (v, args)
eval env (App hd args) = 
    case (hd, args) of
        (Add, e1 Q.:<| e2 Q.:<| Q.Empty) -> do 
            n1 <- eval env e1 >>= ensureNumber
            n2 <- eval env e2 >>= ensureNumber
            pure $ VNumber $ n1 + n2
        (Mul, e1 Q.:<| e2 Q.:<| Q.Empty) -> do 
            n1 <- eval env e1 >>= ensureNumber
            n2 <- eval env e2 >>= ensureNumber
            pure $ VNumber $ n1 * n2
        (Div, e1 Q.:<| e2 Q.:<| Q.Empty) -> do 
            n1 <- eval env e1 >>= ensureNumber
            n2 <- eval env e2 >>= ensureNumber
            pure $ VNumber $ n1 `div` n2
        (Neg, e Q.:<| Q.Empty) -> do
            n <- eval env e >>= ensureNumber
            pure $ VNumber $ negate n
        (B, e1 Q.:<| e2 Q.:<| e3 Q.:<| es) ->
            eval env $ appArgs e1 (app e2 e3 Q.:<| es)
        (C, e1 Q.:<| e2 Q.:<| e3 Q.:<| es)  ->
            eval env $ appArgs e1 (e3 Q.:<| e2 Q.:<| es)
        (S, e1 Q.:<| e2 Q.:<| e3 Q.:<| es) -> do
            t3 <- mkThunkExpr env e3
            let e3' = EThunk t3 Q.Empty
            eval env $ appArgs e1 (e3' Q.:<| app e2 e3' Q.:<| es)
        (I, e1 Q.:<| es) -> eval env (appArgs e1 es)
        (T, e1 Q.:<| _ Q.:<| es) -> 
            eval env (appArgs e1 es)
        (F, _ Q.:<| e2 Q.:<| es) -> 
            eval env (appArgs e2 es)
        (Car, e1 Q.:<| es) -> do
            VCons v1 _ <- eval env e1
            t1 <- mkThunk undefined (pure v1)
            eval env (EThunk t1 es)
        (Car, e1 Q.:<| es) ->  eval env $ appArgs e1 (symToExpr T Q.:<| es)
        (Cdr, e1 Q.:<| es) -> do
            VCons _ v2 <- eval env e1
            t2 <- mkThunk undefined (pure v2)
            eval env (EThunk t2 es)
        (Cdr, e1 Q.:<| es) ->  eval env $ appArgs e1 (symToExpr F Q.:<| es)
        (Nil, Q.Empty) -> pure $ VNil
        (Nil, _ Q.:<| es) -> eval env (appArgs (symToExpr T) es)
        (Cons, e1 Q.:<| e2 Q.:<| Q.Empty) -> do
            v1 <- eval env e1
            v2 <- eval env e2
            pure $ VCons v1 v2
        (Cons, e1 Q.:<| e2 Q.:<| e3 Q.:<| es) ->
            eval env (appArgs e3 (e1 Q.:<| e2 Q.:<| es))
        (IsNil, e1 Q.:<| es) -> do
            v <- eval env e1
            case v of
                VNil -> eval env $ appArgs (symToExpr T) es
                VCons {} -> eval env $ appArgs (symToExpr F) es
                _ -> throwError $ "Nil or Cons is expected but found" ++ show v
        (Lt, e1 Q.:<| e2 Q.:<| es) -> do
            n1 <- eval env e1 >>= ensureNumber
            n2 <- eval env e2 >>= ensureNumber
            if n1 < n2
                then eval env (appArgs (symToExpr T) es)
                else eval env (appArgs (symToExpr F) es)
        (Eq, e1 Q.:<| e2 Q.:<| es) -> do
            n1 <- eval env e1 >>= ensureNumber
            n2 <- eval env e2 >>= ensureNumber
            if n1 == n2
                then eval env (appArgs (symToExpr T) es)
                else eval env (appArgs (symToExpr F) es)
        (NonTerm n, es) -> do -- es [ e_n, ... , e2, e1, e0]
            t <- case M.lookup n env of
                Nothing -> throwError $  "Undefined Nonterminal: " ++ show n
                Just v -> pure v
            eval env (EThunk t es)
        (Num n, Q.Empty) -> pure $ VNumber n
        _ -> VPApp hd <$> forM args (\e -> mkThunkExpr env e) 

mkThunk :: Expr -> ExceptT String IO Value -> ExceptT String IO Thunk
mkThunk e action = liftIO $ Thunk <$> newIORef (Left action)

mkThunkExpr :: Env -> Expr -> ExceptT String IO Thunk
mkThunkExpr _ (EThunk t Q.Empty) = pure t
mkThunkExpr env e = liftIO $ Thunk <$> newIORef (Left $ eval env e)

evalThunk :: Thunk -> ExceptT String IO Value
evalThunk (Thunk ref) = do
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
