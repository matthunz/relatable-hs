{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Relatable where

import Data.Kind
import Data.Proxy
import GHC.TypeLits

type family Lookup (key :: Symbol) (db :: [(Symbol, Type)]) where
  Lookup key ('(key, a) ': db) = a
  Lookup key ('(key', a) ': db) = Lookup key db

data Expr (db :: [(Symbol, Type)]) a where
  ColumnExpr :: (KnownSymbol key) => Proxy key -> Expr db (Lookup key db)
  AddExpr :: Expr db a -> Expr db a -> Expr db a
  LitExpr :: a -> Expr db a

instance (Show a) => Show (Expr db a) where
  show (ColumnExpr p) = symbolVal p
  show (AddExpr e1 e2) = show e1 ++ " + " ++ show e2
  show (LitExpr l) = show l
