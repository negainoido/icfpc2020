module Negainoido.Compile where

import Negainoido.Syntax
import qualified Data.Map as M

type ArityEnv = M.Map NT Int
symArity :: ArityEnv -> Symbol -> Int
symArity _ Add = 2
symArity _ Mul = 2
symArity _ Div = 2
symArity _ Neg = 1
symArity _ Eq = 2
symArity _ Lt = 2
symArity _ B = 3
symArity _ C = 3
symArity _ S = 3
symArity _ I = 1
symArity _ Car = 1
symArity _ Cdr = 1
symArity _ Cons = 2 -- actually 3 but 2 is better
symArity _ Nil = 0  -- actually 1 but 0 is better
symArity _ T = 2
symArity _ F = 2
symArity _ IsNil = 1
symArity _ (Num _) = 0
symArity _ (Var _) = 0
symArity tbl (NonTerm n) = 
    case M.lookup n tbl of
        Just v -> v
        Nothing -> error $ "symArity: no arity for:"  ++ show n
symArity _ Ap = error "symArity is undefined for Ap"

analyseArity :: ArityEnv -> Expr -> Int
analyseArity env (App hd xs) = ar
    where
    hdArity = analyseHeadArity env hd 
    ar = max 0 (hdArity - length xs)
    
analyseHeadArity :: ArityEnv -> Head -> Int
analyseHeadArity env (HSymbol s) = symArity env s
analyseHeadArity _ (HThunk t) = 
    error $ "analyseHeadArity: unexpected thunk: " ++ show t

compileDef :: ArityEnv -> Def -> Def
compileDef env def@(Def hd body arity) 
    | plusArity == 0 = def
    | otherwise = compileDef env (Def hd body' (arity + plusArity))
    where
    plusArity = analyseArity env body 
    body0 = liftIndex plusArity body
    body' = simplify $ 
        foldr (flip app) body0 
            [ App (HSymbol (Var (V x))) [] | x <- [0..plusArity -1]]

liftIndex :: Int -> Expr -> Expr
liftIndex n (App hd args) = App hd' (map (liftIndex n) args)
    where
    hd' = case hd of
        HSymbol (Var (V x)) -> 
            HSymbol (Var (V (x + n)))
        _ -> hd

simplifySizeLimit :: Int
simplifySizeLimit = 100

size :: Expr -> Int
size (App _ args) = 1 + sum (map size args)

simplify :: Expr -> Expr
simplify e | size e > simplifySizeLimit = e
simplify expr@(App hd args) =
    case hd of
        HSymbol B 
          | Just e <- triOp f args' -> simplifyMore e
            where f e0 e1 e2 = app e0 (app e1 e2)
        HSymbol C
          | Just e <- triOp f args' -> simplifyMore e
            where f e0 e1 e2 = app (app e0 e2) e1
        HSymbol S
          | Just e <- triOp f args' -> simplifyMore e
            where f e0 e1 e2 = app (app e0 e2) (app e1 e2)
        _ -> expr
    where
    simplifyMore e 
        | e == expr = e
        | size e > simplifySizeLimit = e
        | otherwise = simplify e
    args' = map simplify args
    triOp f args1
        | e0:e1:e2:es <- reverse args1 = 
            Just $ foldl app (f e0 e1 e2) es
        | otherwise = Nothing