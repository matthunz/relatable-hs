{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Proxy
import Relatable

q :: Expr '[ '("foo", Int)] Int
q = AddExpr (ColumnExpr (Proxy @"foo")) (LitExpr 1)

main :: IO ()
main = print q
