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

type family Lookup (key :: Symbol) (db :: [(Symbol, Type)]) where
  Lookup key ('(key, a) ': db) = a
  Lookup key ('(key', a) ': db) = Lookup key db

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

data Expr (db :: [(Symbol, Type)]) a where
  ColumnExpr :: (KnownSymbol key) => Alias key -> Expr db (Lookup key db)
  LitExpr :: a -> Expr db a
  OpExpr :: Expr db a -> Op -> Expr db a -> Expr db a
  UnaryOpExpr :: UnaryOp -> Expr db a -> Expr db a

instance (Show a) => Show (Expr db a) where
  show (ColumnExpr p) = symbolVal p
  show (LitExpr a) = show a
  show (OpExpr a op b) = "(" ++ show a ++ " " ++ show op ++ " " ++ show b ++ ")"
  show (UnaryOpExpr op a) = show op ++ " " ++ show a

instance (Num a) => Num (Expr db a) where
  fromInteger = LitExpr . fromInteger
  (+) lhs = OpExpr lhs AddOp
  (-) lhs = OpExpr lhs SubOp
  (*) lhs = OpExpr lhs MulOp
  abs = UnaryOpExpr AbsOp
  signum = UnaryOpExpr SignumOp
  negate = UnaryOpExpr NegOp

column :: (KnownSymbol key) => Alias key -> Expr db (Lookup key db)
column = ColumnExpr

lit :: a -> Expr db a
lit = LitExpr

newtype Alias (ident :: Symbol) = Alias (Proxy ident)

instance (ident ~ i) => IsLabel i (Alias ident) where
  fromLabel = Alias Proxy
