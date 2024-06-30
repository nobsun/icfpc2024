
```haskell
((λ v1 -> (λ v2 -> v1 (v2 v2)) (λ v2 -> v1 (v2 v2))) (λ v3 -> λ v4 -> 
if v4 < 2
then 1
else (v3 (v4 - 1)) + (v3 (v4 - 2))
)) 40
```

`(λ v1 -> (λ v2 -> v1 (v2 v2)) (λ v2 -> v1 (v2 v2)))` は Y なので、

```haskell
v3 v4 =
  if v4 < 2
  then 1
  else (v3 (v4 - 1)) + (v3 (v4 - 2))
```

この `v3` は `fib` なので、

```haskell
let fib = 1 : 1 : zipWith (+) fib (tail fib) in fib !! 40
```

で 165580141 が答え。


