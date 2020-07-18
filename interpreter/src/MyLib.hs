{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE DeriveGeneric #-} 
{-# LANGUAGE RecordWildCards #-} 
module MyLib  where

import qualified Data.Text as T
import qualified Data.Map as M
import Control.Monad.Fix
import Data.Text(Text)
import Text.Read
import Data.IORef
import GHC.Generics
import Control.Monad.Except
--import Debug.Trace
import qualified Data.Text.IO as T
import Data.Aeson(ToJSON(..), encodeFile)
import Negainoido.Syntax


parseDef :: Text  -> Except String Def
parseDef txt = Def <$> (parseHead hd) <*> (parseBody body)
    where
    hd: _eq: body = T.words txt
    
parseHead :: Text -> Except String NT
parseHead txt =
    pure $ NT (read (T.unpack (T.tail txt)))

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
          | Just n <- readMaybe (T.unpack x) -> pure $ Num n
        _ -> throwError $ "unknown symbol:" ++ show x


symToExpr :: Symbol -> Expr
symToExpr x = App x []

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
      toStackElem c = SExpr (App c [])

app :: Expr -> Expr -> Expr
app (App c es) e2 = App c (e2:es)
app (EThunk e es) e2 = EThunk e (e2:es) 

type Env = M.Map NT Thunk

evalMain :: [Def] -> Expr -> IO (Either String Result)
evalMain defs expr =  
    runExceptT $ do
        env <- mfix $ \env -> 
            M.fromList <$> (
                forM defs $ \(Def hd body) -> do
                    thunk <- mkThunk body (eval env body)
                    pure (hd, thunk))
        r <- eval env expr >>= evalForce
        liftIO $ putStrLn "raw output is written at result.txt"
        liftIO $ writeFile "result.txt" (show r)
        res@Result {..}  <- dataToResult r
        liftIO $ putStrLn "result json is written at result.json"
        liftIO $ encodeFile "result.json" res
        liftIO $ putStrLn $ "Result: " ++ show returnValue
        liftIO $ T.putStrLn $ "DataAsCode: " <> toCode stateData
        case imageList of 
            Just imageList' -> 
                forM_ (zip [(1 :: Int)..] imageList')  $ \(i, image) -> do
                let filename = "image_" ++ show i ++ ".txt"
                    content = unlines [ show x ++ " "  ++ show y | (x,y) <- image]
                liftIO $ putStrLn $ "image is written at " ++ filename
                liftIO $ writeFile filename $ content
            Nothing -> pure ()
        when (returnValue /= 0) $ liftIO $ T.putStrLn $ "ImageListAsCode: " <> toCode imageListAsData
        pure res



main :: IO ()
main = do
    content <- T.getContents
    let doit = do
            let (gdef: defs) = reverse $ T.lines content 
            defs1 <- mapM parseDef defs
            mainExpr <- parseMain gdef
            pure (defs1, mainExpr)
    case runExcept doit of
        Left err -> putStrLn $ "Error:" ++ err
        Right (defs1, mainExpr) -> do
            res <- evalMain defs1 mainExpr
            case res of
                Left e -> print $ "Error!:" ++ e
                Right _r -> pure ()
        
evalForce :: Value -> ExceptT String IO SData
evalForce (VNumber v) = pure $ DNumber v
evalForce VNil = pure $ DNil
evalForce (VPApp Cons [t2, t1]) = DCons <$> (evalThunk t1 >>= evalForce) <*> (evalThunk t2 >>= evalForce)
evalForce e = throwError $ "cannot force partial application" ++ show e

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
        (Cons, _) | Just e <- triOp f -> eval env e
            where
            f e0 e1 e2 = app (app e2 e0) e1
        (IsNil, _) | Just me <- uniOpM f -> me >>= eval env
            where
            f e = do
                v <- eval env e
                case v of
                    VNil -> pure $ symToExpr T
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
        (Nil, []) -> pure VNil
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

ensureCons :: Value -> ExceptT String IO (Thunk,Thunk)
ensureCons (VCons t1 t2) = pure (t1, t2)
ensureCons e = throwError $ "Cons is expected but found: " ++ show e 

data Result = Result {
    returnValue :: Integer,
    stateData :: SData,
    imageList :: Maybe [[(Integer, Integer)]],
    imageListAsData :: SData
} deriving(Generic, Show, Eq)

instance ToJSON Result

dataToResult :: SData -> ExceptT String IO Result
dataToResult (v1 `DCons` (v2 `DCons` (v3 `DCons` DNil))) = do
    n1 <- case v1 of  
        DNumber n -> pure n
        _ -> throwError $ "first element should be number" ++ show v1 
    let dat = v2
    imageList <- (Just <$> dataToImageList v3) `catchError` (\_ -> pure Nothing)
    pure Result {
        returnValue = n1, 
        stateData = dat,
        imageList = imageList,
        imageListAsData = v3 
        }
dataToResult e = throwError $ "Triplet is expected but found: " <> show e


dataToNumber :: SData -> ExceptT String IO Integer
dataToNumber (DNumber n) = pure n
dataToNumber e = throwError $ "expected number but found :" ++ show e

dataToImageList :: SData -> ExceptT String IO [[(Integer, Integer)]]
dataToImageList (DCons x xs) = (:) <$> dataToImage x <*> dataToImageList xs
dataToImageList DNil = pure []
dataToImageList e = throwError $ "expected nil or cons but found :" ++ show e

dataToImage :: SData -> ExceptT String IO [(Integer, Integer)]
dataToImage (DCons x xs) = (:) <$> dataToPoint x <*> dataToImage xs
dataToImage DNil = pure []
dataToImage e = throwError $ "expected nil or cons but found : " ++ show e

dataToPoint :: SData -> ExceptT String IO (Integer, Integer)
dataToPoint (DCons x y)  = do 
    n1 <- dataToNumber x
    n2 <- dataToNumber y
    pure (n1, n2)
dataToPoint e = throwError $ "expected point but found: " ++ show e
        
