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
import Data.Aeson(ToJSON(..))

data Symbol = Add | Ap | B | C 
  | Car | Cdr | Cons | Div | Eq | I 
  | IsNil | Lt | Mul | Neg | Nil | S | T | F
  | NonTerm NT
  | Num Integer
  | Var V
  deriving (Eq, Show)
newtype NT = NT Int
    deriving (Eq, Ord, Show)
newtype V = V Int
    deriving (Eq, Ord, Show)

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

symToExpr :: Symbol -> Expr
symToExpr x = App (HSymbol x) []


app :: Expr -> Expr -> Expr
app (App c es) e2 = App c (e2:es)