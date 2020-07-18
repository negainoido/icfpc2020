{-# LANGUAGE OverloadedStrings #-} 
module MyLib  where

import qualified Data.Text as T
import qualified Data.Map as M
import Control.Monad.Fix
import Data.Text(Text)
import Text.Read
import Data.IORef
import System.IO.Unsafe
import Control.Monad.Except
import Debug.Trace
import qualified Data.Text.IO as T
import Text.Builder as B

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Symbol = Add | Ap | B | C 
  | Car | Cdr | Cons | Div | Eq | I 
  | IsNil | Lt | Mul | Neg | Nil | S | T | F
  | NonTerm NT
  | Num Integer
  deriving (Eq, Show)
newtype NT = NT Int
    deriving (Eq, Ord, Show)

data Def = Def {
    head :: NT,
    body :: Expr
} deriving(Eq, Show)

parseDef :: Text  -> Def
parseDef txt = Def (parseHead head) (parseBody body)
    where
    head: eq: body = T.words txt
    
parseHead :: Text -> NT
parseHead txt =
    NT (read (T.unpack (T.tail txt)))

parseMain :: Text -> Expr
parseMain = parseBody . T.words

parseBody :: [Text] -> Expr
parseBody = parse . map parseSymbol

parseSymbol :: Text -> Symbol
parseSymbol x = 
    case x of
        "add" ->  Add
        "ap" -> Ap
        "b" -> B
        "c" -> C
        "car" -> Car
        "cdr" -> Cdr
        "cons" -> Cons
        "div" -> Div
        "eq" -> Eq
        "i" -> I
        "isnil" -> IsNil
        "lt" -> Lt
        "mul" -> Mul
        "neg" -> Neg
        "nil" -> Nil
        "s" -> S
        "t" -> T
        "f" -> F
        x | T.head x == ':' -> NonTerm (parseHead x)
          | Just n <- readMaybe (T.unpack x) -> Num n
        _ -> error $ "unknown symbol:" ++ show x

data Expr = App Symbol [Expr] -- arguments are in reverse order
    | EThunk Thunk [Expr]
    deriving (Eq)

instance Show Expr where
    show (App x []) = show x
    show (App x es) = "(" ++ unwords (show x: map show (reverse es)) ++ ")"
    show (EThunk t es) =
        "(Thunk " ++ unwords (show t: map show (reverse es)) ++ ")"

symToExpr :: Symbol -> Expr
symToExpr x = App x []

data StackElem = SExpr Expr | SAp 

parse :: [Symbol] -> Expr
parse = go []
  where
      go :: [StackElem] -> [Symbol] -> Expr
      go (SExpr c1: SExpr c2: SAp: st) syms = 
          go (SExpr (app c2 c1):st) syms
      go st (c:syms) = go (toStackElem c: st) syms
      go [SExpr e] [] = e
      toStackElem Ap = SAp
      toStackElem c = SExpr (App c [])

app :: Expr -> Expr -> Expr
app (App c es) e2 = App c (e2:es)
app (EThunk e es) e2 = EThunk e (e2:es) 

data Thunk = Thunk Expr (IORef (Either (ExceptT String IO Value) Value))
    deriving(Eq)

instance Show Thunk where
    show (Thunk e ref) = unsafePerformIO $ do
        r <- readIORef ref
        case r of
            Left _ -> pure (show e)
            Right v -> pure $ show v


data Value = 
      VNumber Integer
    | VCons Thunk Thunk
    | VNil
    | VPApp Symbol [Thunk] -- arguments are in reverse order
    deriving(Show, Eq)

data SData = DNumber Integer | DCons SData SData | DNil
    deriving(Eq)

instance Show SData where
    show (DNumber i) = show i
    show (DCons v1 v2) = "(" ++ unwords ["cons", show v1, show v2] ++  ")"
    show DNil = "nil"

type Env = M.Map NT Thunk

evalMain :: [Def] -> Expr -> IO (Either String SData)
evalMain defs expr =  
    runExceptT $ do
        env <- mfix $ \env -> 
            M.fromList <$> (
                forM defs $ \(Def head body) -> do
                    thunk <- mkThunk body (eval env body)
                    pure (head, thunk))
        r <- eval env expr >>= evalForce
        catchError (do
            Result r dat imageList imageListAsData <- dataToResult r
            liftIO $ putStrLn $ "Result: " ++ show r
            --liftIO $ putStrLn $ "Data: " ++ show dat
            liftIO $ T.putStrLn $ "DataAsCode: " <> toCode dat
            case imageList of 
                Just imageList -> 
                    forM_ (zip [(1 :: Int)..] imageList)  $ \(i, image) -> do
                    let filename = "image_" ++ show i ++ ".txt"
                        content = unlines [ show x ++ " "  ++ show y | (x,y) <- image]
                    liftIO $ writeFile filename $ content
                Nothing -> pure ()
            when (r /= 0) $ liftIO $ T.putStrLn $ "ImageListAsCode: " <> toCode imageListAsData
            ) (\e -> liftIO $ putStrLn $ "Error: " ++ e)
        return r

toCode :: SData -> Text
toCode = B.run . go
    where
    sp = B.char ' '
    go (DCons a b) = "ap" <> sp <> "ap" <> sp <> "cons" <> sp <> go a <> sp <> go b
    go DNil = "nil"
    go (DNumber n) = B.decimal n

main :: IO ()
main = do
    content <- T.getContents
    let (gdef: defs) = reverse $ T.lines content
        defs1 = map parseDef defs
        mainExpr = parseMain gdef
    res <- evalMain defs1 mainExpr

    case res of
        Left e -> print $ "Error!:" ++ e
        Right r -> pure ()
        

         

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
        (VPApp head args', args) -> 
            let toExpr x = EThunk x [] in
            eval env (App head (args ++ (map toExpr args')))
        (v, []) -> pure v
        (v, args) -> throwError $ "cannot apply: " ++ show (v, args)
eval env (App head args) = 
    let triOp f args 
            | e0:e1:e2:es <- reverse args = Just $ foldl app (f e0 e1 e2) es  
            | otherwise = Nothing
        triOpM f args 
            | e0:e1:e2:es <- reverse args = Just $ do
                e' <- f e0 e1 e2
                pure $ foldl app e' es  
            | otherwise = Nothing 
        uniOp f args
            | e0: es <- reverse args = Just $ foldl app (f e0) es
            | otherwise = Nothing
        uniOpM f args
            | e0: es <- reverse args = Just $ do
                e' <- f e0
                pure $ foldl app e' es
            | otherwise = Nothing
        binOp f args
            | e0:e1: es <- reverse args = Just $ foldl app (f e0 e1) es
            | otherwise = Nothing
        binOpM f args
            | e0:e1: es <- reverse args = Just $ do
                 e' <- f e0 e1
                 pure $ foldl app e' es
            | otherwise = Nothing
            in
    case (head, args) of
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
        (B, _) | Just e <- triOp f args -> eval env e
            where f e0 e1 e2 = app e0 (app e1 e2)
        (C, _) | Just e <- triOp f args -> eval env e
            where f e0 e1 e2 = app (app e0 e2) e1
        (S, _) | Just me <- triOpM f args -> me >>= eval env
            where
            f e0 e1 e2 =  do
                t2 <- mkThunk e2 (eval env e2)
                let e2' = EThunk t2 []
                pure $ app (app e0 e2') (app e1 e2')
        (I, _) | Just e <- uniOp id args ->  eval env e
        (T, _) | Just e <- binOp (\x y -> x) args -> eval env e
        (F, _) | Just e <- binOp (\x y -> y) args -> eval env e
        (Car, _) | Just e <- uniOp f args ->  eval env e
            where
            f e = app e (symToExpr T)
        (Cdr, _) | Just e <- uniOp f args ->  eval env e
            where
            f e = app e (symToExpr F)
        (Cons, _) | Just e <- triOp f args -> eval env e
            where
            f e0 e1 e2 = app (app e2 e0) e1
        (IsNil, _) | Just me <- uniOpM f args -> me >>= eval env
            where
            f e = do
                v <- eval env e
                case v of
                    VNil -> pure $ symToExpr T
                    VPApp Cons _ -> pure $ symToExpr F
                    _ -> throwError $ "Nil or Cons is expected but found" ++ show v
        (Lt, _) | Just me <- binOpM f args -> me >>= eval env
            where
                f e1 e2 = do
                    n1 <- eval env e1 >>= ensureNumber
                    n2 <- eval env e2 >>= ensureNumber
                    pure $ if n1 < n2
                        then symToExpr T
                        else symToExpr F
        (Eq, _) | Just me <- binOpM f args -> me >>= eval env
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
        (head, args) -> 
            VPApp head <$> forM args (\e -> mkThunk e (eval env e)) 

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
}

dataToResult :: SData -> ExceptT String IO Result
dataToResult (v1 `DCons` (v2 `DCons` (v3 `DCons` DNil))) = do
    n1 <- case v1 of  
        DNumber n -> pure n
        _ -> throwError $ "first element should be number" ++ show v1 
    let dat = v2
    imageList <- (Just <$> dataToImageList v3) `catchError` (\e -> pure Nothing)
    pure Result {
        returnValue = n1, 
        stateData = dat,
        imageList = imageList,
        imageListAsData = v3 
        }


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
        
