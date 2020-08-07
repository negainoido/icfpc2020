{-# LANGUAGE OverloadedStrings #-} 
module Negainoido.Syntax where

import Data.Text(Text)
import Data.IORef
import System.IO.Unsafe
import Control.Monad.Except
--import Debug.Trace
import Text.Builder as B
import Data.Aeson(ToJSON(..))
import qualified Data.Sequence as Q
import Data.Foldable

data Symbol = Add | Ap | B | C 
  | Car | Cdr | Cons | Div | Eq | I 
  | IsNil | Lt | Mul | Neg | Nil | S | T | F
  | NonTerm NT
  | Num Integer
  deriving (Eq, Show)
newtype NT = NT Int
    deriving (Eq, Ord, Show)

data Def = Def {
    defHead :: NT,
    defBody :: Expr
} deriving(Eq, Show)

data Expr = App Symbol (Q.Seq Expr) 
    | EThunk Thunk (Q.Seq Expr)
    deriving (Eq)

instance Show Expr where
    show (App x Q.Empty) = show x
    show (App x es) = "(" ++ unwords (show x: map show (toList es)) ++ ")"
    show (EThunk t es) =
        "(Thunk " ++ unwords (show t: map show (toList es)) ++ ")"

data Thunk = Thunk Expr (IORef (Either (ExceptT String IO Value) Value))
    deriving(Eq)


instance Show Thunk where
    show (Thunk e ref) = unsafePerformIO $ do
        r <- readIORef ref
        case r of
            Left _ -> pure "_"
            Right v -> pure $ show v

data Value = 
      VNumber Integer
    | VPApp Symbol (Q.Seq Thunk) -- arguments are in reverse order
    deriving(Show, Eq)

data SData = DNumber Integer | DCons SData SData | DNil
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
symToExpr x = App x Q.Empty 


app :: Expr -> Expr -> Expr
app (App c es) e2 = App c (es Q.:|> e2)
app (EThunk e es) e2 = EThunk e (es Q.:|> e2) 