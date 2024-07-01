```haskell
((λ v1 -> (λ v2 -> v1 (v2 v2)) (λ v2 -> v1 (v2 v2))) (λ v3 -> λ v4 -> ((λ v1 -> λ v2 -> 
if v1 < v2
then v1
else v2
) v4) (1 + (
if v4 > 2
then (((λ v1 -> (λ v2 -> v1 (v2 v2)) (λ v2 -> v1 (v2 v2))) (λ v5 -> λ v6 -> λ v7 -> 
 if v6 = v4
 then v7
 else (v5 (v6 + 1)) (
  if (v3 v6) > (v6 - 1)
  then 
   if (v4 % v6) = 0
   then (v7 / (v3 v6)) * ((v3 v6) - 1)
   else v7

  else v7
)
)) 2) v4
else v4
)))) 1234567
```

```haskell
(Y (λ v3 -> λ v4 -> ((λ v1 -> λ v2 -> 
if v1 < v2
then v1
else v2
) v4) (1 + (
if v4 > 2
then (((λ v1 -> (λ v2 -> v1 (v2 v2)) (λ v2 -> v1 (v2 v2))) (λ v5 -> λ v6 -> λ v7 -> 
 if v6 = v4
 then v7
 else (v5 (v6 + 1)) (
  if (v3 v6) > (v6 - 1)
  then 
   if (v4 % v6) = 0
   then (v7 / (v3 v6)) * ((v3 v6) - 1)
   else v7

  else v7
)
)) 2) v4
else v4
)))) 1234567
```

```haskell
let v3 v4 = min v4 (1 + foo)
      where
        min v1 v2 = if v1 < v2 then v1 else v2
        foo =
         if v4 > 2 then
           let v5 v6 v7 = 
                 if v6 == v4 then
                   v7
                 else
                   v5 (v6 + 1) $
                     if v3 v6 > v6 - 1 then 
                       if v4 % v6 == 0 then
                         (v7 / (v3 v6)) * ((v3 v6) - 1)
                       else
                         v7
                     else
                       v7
            in v5 2 v4
         else
           v4
 in v3 1234567
```

```haskell
f :: Integer -> Integer
f x = min x (1 + (if x > 2 then g 2 x else x))
  where
    g !y !z
      | y == x = z
      | otherwise = g (y + 1) z'
      where
        fy = f y
        z' = if (fy > y - 1) && (x `rem` y == 0) then
               (z `quot` fy) * (fy - 1)
             else
               z

main = print $ f 1234567
```
