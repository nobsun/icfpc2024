```haskell
(λ v6 -> (λ v7 -> ((λ v1 -> (λ v2 -> v1 (v2 v2)) (λ v2 -> v1 (v2 v2))) (λ v3 -> λ v4 -> 
if (v4 > 1000000) & ((v6 v4) & (v7 (v4 + 1)))
then v4
else v3 (v4 + 1)
)) 2) ((λ v1 -> (λ v2 -> v1 (v2 v2)) (λ v2 -> v1 (v2 v2))) (λ v3 -> λ v4 -> 
if v4 = 1
then true
else 
 if (v4 % 2) = 1
 then false
 else v3 (v4 / 2)

))) (λ v5 -> ((λ v1 -> (λ v2 -> v1 (v2 v2)) (λ v2 -> v1 (v2 v2))) (λ v3 -> λ v4 -> 
if v4 = v5
then true
else 
 if (v5 % v4) = 0
 then false
 else v3 (v4 + 1)

)) 2)
```

`Y = (λ v1 -> (λ v2 -> v1 (v2 v2)) (λ v2 -> v1 (v2 v2)))` とおくと、

```haskell
(λ v6 -> (λ v7 -> (Y (λ v3 -> λ v4 -> 
if (v4 > 1000000) & ((v6 v4) & (v7 (v4 + 1)))
then v4
else v3 (v4 + 1)
)) 2) (Y (λ v3 -> λ v4 -> 
if v4 = 1
then true
else 
 if (v4 % 2) = 1
 then false
 else v3 (v4 / 2)

))) (λ v5 -> (Y (λ v3 -> λ v4 -> 
if v4 = v5
then true
else 
 if (v5 % v4) = 0
 then false
 else v3 (v4 + 1)

)) 2)
```

```haskell
f v6 = v3 2
  where
    v3 v4 =
      if (v4 > 1000000) & ((v6 v4) & (v7 (v4 + 1)))
      then v4
      else v3 (v4 + 1)
    v7 v4 =
      if v4 == 1
      then true
      else 
       if (v4 % 2) = 1
       then false
       else v7 (v4 / 2)

g v5 = v3 2
  where
    v3 v4 = 
      if v4 = v5
      then true
      else 
       if (v5 % v4) = 0
       then false
       else v3 (v4 + 1)

ans = f g
```
