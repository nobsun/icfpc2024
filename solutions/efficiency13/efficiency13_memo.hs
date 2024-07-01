{-
((\ v1 -> (\ v2 -> v1 (v2 v2)) (\ v2 -> v1 (v2 v2)))
 (\ v3 v4 ->
     if v4 == ""
     then 0
     else 1 + (v3 (drop 1 v4))
 ))
((\ v2 ->
    (v2 (v2 (v2 (v2 (v2 (v2 (v2 (v2 (v2 (v2 (v2 (v2 (v2 (v2 (v2 (v2 (v2 (v2 (v2 (v2 (v2 (v2 (v2 (v2 (v2 (v2 (v2 (v2 "na")))))))))))))))))))))))))))) <> "heyjude")
 (\ v1 -> v1 <> v1))
 -}

main :: IO ()
main = pure ()

f :: String -> Int
f v4 =
  if v4 == ""
  then 0
  else 1 + (f (drop 1 v4))

g :: String -> String
g v1 = v1 <> v1

h :: String
h = (g (g (g (g (g (g (g (g (g (g (g (g (g (g (g (g (g (g (g (g (g (g (g (g (g (g (g (g "na")))))))))))))))))))))))))))) <> "heyjude"

{- g は文字列の長さを 2倍にする.
   長さ 2 の文字列を 28回 2倍にするので、2^29.
   "heyjude" は長さ 7.
   最終的な長さは 2^29 + 7 == 536870919.
   f は再帰で文字列の長さを測る  -}
