# Relatable

```hs
import Relatable

type Users = '["name" ::: Int]

type DB = '[Table "users" Users]

query :: Stmt DB Users '[Int]
query = select (column #name `eq` lit 1) . project (column #name + lit 1) $ table #users

main :: IO ()
main = print query
```