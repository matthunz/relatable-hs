# Relatable

```hs
import Relatable

q :: Expr '["name" ::: Int] Int
q = column #name + lit 1

f :: Expr '["name" ::: Int] Bool
f = column #name `eq` lit 1

s :: Stmt '[Table "users" '["name" ::: Int]] '["name" ::: Int] '[Int]
s = select f . project q $ table #users

main :: IO ()
main = print s
```