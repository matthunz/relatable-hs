{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Relatable where

import Data.Kind
import Data.Proxy
import GHC.OverloadedLabels
import GHC.TypeLits

newtype Alias (ident :: Symbol) = Alias (Proxy ident)

instance (ident ~ i) => IsLabel i (Alias ident) where
  fromLabel = Alias Proxy

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

data RowT f as where
  Nil :: RowT f '[]
  Cons :: f a -> RowT f as -> RowT f (a ': as)

instance Show (RowT f '[]) where
  show Nil = ""

instance (Show (f a), Show (RowT f as)) => Show (RowT f (a ': as)) where
  show (Cons a as) = show a ++ rest
    where
      rest = case as of
        Nil -> ""
        _ -> ", " ++ show as

class ToRow (f :: Type -> Type) a where
  type ToRowT f a :: [Type]

  toRow :: a -> RowT f (ToRowT f a)

instance ToRow (Expr env) (Expr env a) where
  type ToRowT (Expr env) (Expr env a) = '[a]

  toRow a = Cons a Nil

data a :& b = a :& b

type family (as :: [Type]) ++ (bs :: [Type]) :: [Type] where
  '[] ++ bs = bs
  (a ': as) ++ bs = a ': (as ++ bs)

class Append (as :: [Type]) (bs :: [Type]) where
  append :: RowT f as -> RowT f bs -> RowT f (as ++ bs)

instance Append '[] bs where
  append Nil bs = bs

instance (Append as bs) => Append (a ': as) bs where
  append (Cons a as) bs = Cons a (append as bs)

instance (ToRow f a, ToRow f b, Append (ToRowT f a) (ToRowT f b)) => ToRow f (a :& b) where
  type ToRowT f (a :& b) = ToRowT f a ++ ToRowT f b

  toRow (a :& b) = toRow a `append` toRow b

data Table (ident :: Symbol) (cols :: [Type]) = Table (Proxy ident) (Proxy cols)

type family LookupTable (key :: Symbol) (db :: [Type]) where
  LookupTable key (Table key cols ': db) = cols
  LookupTable key (x ': db) = LookupTable key db

data Stmt (db :: [Type]) (a :: [Type]) where
  NullStmt :: Stmt db a
  ProjectStmt :: (Show (RowT (Expr b) a)) => RowT (Expr b) a -> Stmt db b -> Stmt db a
  TableStmt :: (KnownSymbol ident) => Alias ident -> Stmt db (LookupTable ident db)

instance Show (Stmt db a) where
  show NullStmt = "∅"
  show (ProjectStmt row stmt) = "Π[" ++ show row ++ "](" ++ show stmt ++ ")"
  show (TableStmt t) = symbolVal t

empty :: Stmt db a
empty = NullStmt

project ::
  (ToRow (Expr env) a, Show (RowT (Expr env) (ToRowT (Expr env) a))) =>
  a ->
  Stmt db env ->
  Stmt db (ToRowT (Expr env) a)
project = ProjectStmt . toRow

table :: (KnownSymbol ident) => Alias ident -> Stmt db (LookupTable ident db)
table = TableStmt