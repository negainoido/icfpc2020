{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
module Negainoido.Syntax where

import Data.Text(Text)
import Data.IORef
import System.IO.Unsafe
import Control.Monad.Except
--import Debug.Trace
import Text.Builder as B
import Data.Char
import Data.Aeson(ToJSON(..))

data Symbol = Add | Ap | B | C 
  | Car | Cdr | Cons | Div | Eq | I 
  | IsNil | Lt | Mul | Neg | Nil | S | T | F
  | NonTerm NT
  | Num Integer
  | Var V
  deriving (Eq)

instance Show Symbol where
    show Add = "add"
    show Ap = "ap"
    show B = "b"
    show C = "c"
    show Car = "car"
    show Cdr = "cdr"
    show Cons = "cons"
    show Div = "div"
    show Eq = "eq"
    show I = "i"
    show IsNil = "isnil"
    show Lt = "lt"
    show Mul = "mul"
    show Neg = "neg"
    show Nil = "nil"
    show S = "s"
    show T = "t"
    show F = "f"
    show (NonTerm x) = show x
    show (Num i) = show i
    show (Var v) = show v

newtype NT = NT Int
    deriving (Eq, Ord)
newtype V = V Int
    deriving (Eq, Ord)

instance Show NT where
    show (NT n) = ":" ++ show n
instance Show V where
    show (V n) = "!" ++ show n

data Def = Def {
    defHead  :: !NT,
    defBody  :: !Expr,
    defArity :: !Int
} deriving(Eq, Show)

data Expr = App !Head [Expr] -- arguments are in reverse order
    deriving (Eq)

pattern EThunk :: Thunk -> [Expr] -> Expr
pattern EThunk t xs = App (HThunk t) xs

data Head = 
    HSymbol !Symbol
    | HThunk !Thunk
    deriving (Eq)

instance Show Expr where
    show (App x []) = show x
    show (App x es) = "(" ++ unwords (show x: map show (reverse es)) ++ ")"

instance Show Head where
    show (HSymbol x) = show x
    show (HThunk t) = "(Thunk " ++ show t ++ ")"

data Thunk = Thunk Expr (IORef (Either (ExceptT String IO Value) Value))
    deriving(Eq)


instance Show Thunk where
    show (Thunk e ref) = unsafePerformIO $ do
        r <- readIORef ref
        case r of
            Left _ -> pure "_"
            Right v -> pure $ show v

data Value = 
      VNumber !Integer
    | VPApp !Symbol [Thunk] -- arguments are in reverse order
    deriving(Show, Eq)

data SData = DNumber !Integer | DCons !SData !SData | DNil
    deriving(Eq)

instance Show SData where
    show (DNumber i) = show i
    show (DCons v1 v2) = "(" ++ unwords ["cons", show v1, show v2] ++  ")"
    show DNil = "nil"

instance ToJSON SData where
    toEncoding d = toEncoding (toCode d)
    toJSON d = toJSON (toCode d)

toCode :: SData -> Text
toCode = B.run . go
    where
    sp = B.char ' '
    go (DCons a b) = "ap" <> sp <> "ap" <> sp <> "cons" <> sp <> go a <> sp <> go b
    go DNil = "nil"
    go (DNumber n) = B.decimal n

toCodeExpr :: Expr -> Text
toCodeExpr = B.run . go
    where
    sp = B.char ' '
    go (App hd args) = 
        foldr (\x acc -> "ap" <> sp <> acc <> sp <> go x) (toCodeHead hd) args
toCodeHead :: Head -> Builder
toCodeHead (HSymbol x) = B.string (map toLower (show x))

symToExpr :: Symbol -> Expr
symToExpr x = App (HSymbol x) []


app :: Expr -> Expr -> Expr
app (App c es) e2 = App c (e2:es)