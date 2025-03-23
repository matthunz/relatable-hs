{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Relatable

q :: Expr '["foo" ::: Int] Int
q = column #foo + lit 1

s :: Stmt '[Table "users" '["foo" ::: Int]] '[Int]
s = project q $ table #users

main :: IO ()
main = print s
