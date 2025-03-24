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

data Op b a where
  AddOp :: Op b Int
  SubOp :: Op b Int
  MulOp :: Op b Int
  DivOp :: Op b Int
  EqOp :: Op b Bool
  NeqOp :: Op b Bool
  LtOp :: Op b Bool
  LteOp :: Op b Bool
  GtOp :: Op b Bool
  GteOp :: Op b Bool

instance Show (Op b a) where
  show AddOp = "+"
  show SubOp = "-"
  show MulOp = "*"
  show DivOp = "/"
  show EqOp = "="
  show NeqOp = "!="
  show LtOp = "<"
  show LteOp = "<="
  show GtOp = ">"
  show GteOp = ">="

data UnaryOp = AbsOp | NegOp | NotOp | SignumOp

instance Show UnaryOp where
  show AbsOp = "ABS"
  show NegOp = "-"
  show NotOp = "NOT"
  show SignumOp = "SIGNUM"

data Expr (env :: [Type]) (a :: Type) where
  ColumnExpr :: (KnownSymbol key) => Alias key -> Expr env (Lookup key env)
  LitExpr :: a -> Expr env a
  OpExpr :: (Show b) => Expr env b -> Op b a -> Expr env b -> Expr env a
  UnaryOpExpr :: UnaryOp -> Expr env a -> Expr env a

instance (Show a) => Show (Expr env a) where
  show (ColumnExpr p) = symbolVal p
  show (LitExpr a) = show a
  show (OpExpr a op b) = "(" ++ show a ++ " " ++ show op ++ " " ++ show b ++ ")"
  show (UnaryOpExpr op a) = show op ++ " " ++ show a

instance Num (Expr env Int) where
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

eq :: (Show a) => Expr env a -> Expr env a -> Expr env Bool
eq a = OpExpr a EqOp

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

type family Columns cols where
  Columns '[] = '[]
  Columns (a ::: b ': cols) = b ': Columns cols

type family LookupTable (key :: Symbol) (db :: [Type]) where
  LookupTable key (Table key cols ': db) = cols
  LookupTable key (x ': db) = LookupTable key db

type family LookupTableCols (key :: Symbol) (db :: [Type]) where
  LookupTableCols key (Table key cols ': db) = Columns cols
  LookupTableCols key (x ': db) = LookupTableCols key db

data Stmt (db :: [Type]) (env :: [Type]) (a :: [Type]) where
  NullStmt :: Stmt db env a
  ProjectStmt :: (Show (RowT (Expr env) a)) => RowT (Expr env) a -> Stmt db env b -> Stmt db env a
  SelectStmt :: Expr env' Bool -> Stmt db env a -> Stmt db env' a
  TableStmt :: (KnownSymbol ident) => Alias ident -> Stmt db (LookupTable ident db) (LookupTableCols ident db)

instance Show (Stmt db env a) where
  show NullStmt = "∅"
  show (ProjectStmt row stmt) = "Π[" ++ show row ++ "](" ++ show stmt ++ ")"
  show (SelectStmt cond stmt) = "σ[" ++ show cond ++ "](" ++ show stmt ++ ")"
  show (TableStmt t) = symbolVal t

empty :: Stmt db env a
empty = NullStmt

project ::
  (ToRow (Expr env) a, Show (RowT (Expr env) (ToRowT (Expr env) a))) =>
  a ->
  Stmt db env b ->
  Stmt db env (ToRowT (Expr env) a)
project = ProjectStmt . toRow

select :: Expr env Bool -> Stmt db env' b -> Stmt db env b
select = SelectStmt

table :: (KnownSymbol ident) => Alias ident -> Stmt db (LookupTable ident db) (LookupTableCols ident db)
table = TableStmt
