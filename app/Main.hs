{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Relatable

q :: Expr '["foo" ::: Int] Int
q = column #foo + lit 1

f :: Expr '["foo" ::: Int] Bool
f = column #foo `eq` lit 1

s :: Stmt '[Table "users" '["foo" ::: Int]] '["foo" ::: Int] '[Int]
s = select f . project q $ table #users

main :: IO ()
main = print s
