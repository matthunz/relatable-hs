{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Relatable where

import Data.Kind
import Data.Proxy
import GHC.OverloadedLabels
import GHC.TypeLits

data (a :: Symbol) ::: b = (Alias a) ::: b

type family Lookup (key :: Symbol) (env :: [Type]) where
  Lookup key (a ::: b ': env) = b
  Lookup key (x ': env) = Lookup key env

data Op = AddOp | SubOp | MulOp | DivOp

instance Show Op where
  show AddOp = "+"
  show SubOp = "-"
  show MulOp = "*"
  show DivOp = "/"

data UnaryOp = AbsOp | NegOp | NotOp | SignumOp

instance Show UnaryOp where
  show AbsOp = "ABS"
  show NegOp = "-"
  show NotOp = "NOT"
  show SignumOp = "SIGNUM"

data Expr (env :: [Type]) (a :: Type) where
  ColumnExpr :: (KnownSymbol key) => Alias key -> Expr env (Lookup key env)
  LitExpr :: a -> Expr env a
  OpExpr :: Expr env a -> Op -> Expr env a -> Expr env a
  UnaryOpExpr :: UnaryOp -> Expr env a -> Expr env a

instance (Show a) => Show (Expr env a) where
  show (ColumnExpr p) = symbolVal p
  show (LitExpr a) = show a
  show (OpExpr a op b) = "(" ++ show a ++ " " ++ show op ++ " " ++ show b ++ ")"
  show (UnaryOpExpr op a) = show op ++ " " ++ show a

instance (Num a) => Num (Expr env a) where
  fromInteger = LitExpr . fromInteger
  (+) lhs = OpExpr lhs AddOp
  (-) lhs = OpExpr lhs SubOp
  (*) lhs = OpExpr lhs MulOp
  abs = UnaryOpExpr AbsOp
  signum = UnaryOpExpr SignumOp
  negate = UnaryOpExpr NegOp

column :: (KnownSymbol key) => Alias key -> Expr env (Lookup key env)
column = ColumnExpr

lit :: a -> Expr env a
lit = LitExpr

newtype Alias (ident :: Symbol) = Alias (Proxy ident)

instance (ident ~ i) => IsLabel i (Alias ident) where
  fromLabel = Alias Proxy
