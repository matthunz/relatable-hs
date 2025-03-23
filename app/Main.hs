{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Relatable

q :: Expr '["foo" ::: Int] Int
q = column #foo + lit 1

main :: IO ()
main = print q
