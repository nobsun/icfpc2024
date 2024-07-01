{-
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
-}

-- `Y = (λ v1 -> (λ v2 -> v1 (v2 v2)) (λ v2 -> v1 (v2 v2)))` とおくと、

{-
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
-}

{- --
f :: (Integer -> Bool) -> Integer
f v6 = v3 (2 :: Integer)
  where
    v3 v4 =
      if (v4 > 1000000) && ((v6 v4) && (v7 (v4 + 1)))
      then v4
      else v3 (v4 + 1)
    v7 v4 =
      if v4 == 1
      then True
      else 
       if (v4 `rem` 2) == 1
       then False
       else v7 (v4 `quot` 2)

g v5 = v3 2
  where
    v3 v4 = 
      if v4 == v5
      then True
      else 
       if (v5 `rem` v4) == 0
       then False
       else v3 (v4 + 1)

ans = f g
-}
import Data.List
import Debug.Trace

ub :: Int
ub = 1000000

f = f' ub

f' :: Int -> (Int -> Bool) -> Int
f' u p = h 2
  where
    h m = if m > u && p m && k (m + 1)
                then m
                else h (m + 1)
    k n = if n == 1
        then True
        else 
            if n `rem` 2 == 1
                then False
                else k (n `quot` 2)

g :: Int -> Bool
g n = h 2
  where
    h m = 
      if m == n
        then True
        else if n `rem` m == 0
            then False
            else h (m + 1)

ans = f g

main = print $ map length $ group 
     $ takeWhile (2^13 >) $ flip f' g <$> [1 ..]

-- A232205 ↑
-- A000686 メルセンヌ素数
-- 答えは8番目のメルセンヌ素数
-- ぐぐりました ^^;


