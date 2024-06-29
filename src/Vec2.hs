{- 二次元ベクトル

整数で閉じると計算が1桁くらい高速であるメリットがあるため、できる限り sqrt を取らないものを準備
  ベクトル加算減算  <+>, <->
  内積, ノルム, 外積  |.|, norm2, |*|
  直線
  直線と点の距離  closer
  線分の交差判定  cross
 -}
module Vec2 where

import Data.Coerce (coerce)

{- |
法線ベクトルを nv を持ち点 p を通る直線を L としたとき、
closer (nv, p) q t は
{直線L と点q の距離} が t より近いかを判定する
- 等しいとき  True
- 近いとき    True
- それ以外    False

>>> closer ( Line ((-3, 4), (-4, 0)) ) (0, 0) 2
False
 -}
closer :: (Num a, Ord a) => Line a -> (a, a) -> a -> Bool
closer = closer' coerce

{- |
法線ベクトルを nv を持ち点 p を通る直線を L としたとき、
judgeDist (nv, p) q t は
{直線L と点q の距離} と t を比較するための判別式を返す
判別式: ( nv |.| (p <-> q) )^2 - norm2 nv * t^2
- 等しいときは 0
- q が近いときは負
- q が遠いときは正

>>> judgeDist ( Line ((-3, 4), (-4, 0)) ) (0, 0) 2 > 0
True
 -}
judgeDist :: Num a => Line a -> (a, a) -> a -> a
judgeDist = judgeDist' coerce
{-# SPECIALIZE judgeDist :: Line Double -> (Double, Double) -> Double -> Double #-}
{-# SPECIALIZE judgeDist :: Line Int -> (Int, Int) -> Int -> Int #-}


{- |
距離 t が整数でなくともよい
座標が整数なら、たとえば
closer :: Line Double -> (Double, Double) -> Double -> Bool
よりも高速
 -}
closerNI :: (Integral a, Num b, Ord b) => Line a -> (a, a) -> b -> Bool
closerNI = closer' fromIntegral
{-# SPECIALIZE closerNI :: (Num b, Ord b) => Line Int -> (Int, Int) -> b -> Bool #-}

{- |
距離 t が整数でなくともよい
座標が整数なら、たとえば
judgeDist :: Line Double -> (Double, Double) -> Double -> Double
よりも高速

>>> judgeDistNI ( Line ((-3, 4), (-4, 0)) ) (0, 0) 2 > 0
True
>>> judgeDistNI ( Line ((-3, 4), (-4, 0)) ) (0, 0) 3 < 0
True
>>> judgeDistNI ( Line ((-3, 4), (-4, 0)) ) (0, 0) 2.4
0.0
 -}
judgeDistNI :: (Integral a, Num b) => Line a -> (a, a) -> b -> b
judgeDistNI = judgeDist' fromIntegral
{-# SPECIALIZE judgeDistNI :: Num b => Line Int -> (Int, Int) -> b -> b #-}

{- |
>>> closer' coerce ( Line ((-3, 4), (-4, 0)) ) (0, 0) 2
False
>>> closer' coerce ( Line ((-3, 4), (-4, 0)) ) (0, 0) 3
True
>>> closer' fromIntegral ( Line ((-3, 4), (-4, 0)) ) (0, 0) 2.39
False
>>> closer' fromIntegral ( Line ((-3, 4), (-4, 0)) ) (0, 0) 2.41
True
 -}
closer' :: (Num a, Num b, Ord b) => (a -> b) -> Line a -> (a, a) -> b -> Bool
closer' convAB el q t = judgeDist' convAB el q t <= 0
{-# SPECIALIZE closer' :: (Num b, Ord b) => (Int -> b) -> Line Int -> (Int, Int) -> b -> Bool #-}

judgeDist' :: (Num a, Num b) => (a -> b) -> Line a -> (a, a) -> b -> b
judgeDist' convAB (Line (nv, p)) q t = convAB lhs - rhs
  where
    lhs = square ( nv |.| (p <-> q) )
    rhs = convAB (norm2 nv) * t * t
    square :: Num a => a -> a
    square x = x * x
    {-# SPECIALIZE square :: Double -> Double #-}
    {-# SPECIALIZE square :: Int -> Int #-}
{-# SPECIALIZE judgeDist' :: Num b => (Double -> b) -> Line Double -> (Double, Double) -> b -> b #-}
{-# SPECIALIZE judgeDist' :: Num b => (Int -> b) -> Line Int -> (Int, Int) -> b -> b #-}
{-
   直線L と点q の距離の判別式についての議論

   以下 (・) は内積

   | nv ・ (p - q) | ==
     {- 内積 -}
   |nv| * |p - q| * |cons α| ==
     {- 直線と q の距離を qd とすると、
        qd は法線 nv への正射影の長さとなり、 qd = |p - q| * |cons α|  -}
   |nv| * qd

   qd `compare` t  ⇔
     {- 両辺は正、|nv| を掛ける -}
   |nv| * qd `compare` |nv| * t  ⇔
     {- 両辺は正なので二乗してよい -}
   ( |nv| * qd )^2  `compare` |nv|^2 * t^2  ⇔
     {- |nv| * qd == | nv ・ (p - q) | であった -}
   | nv ・ (p - q) |^2  `compare` |nv|^2 * t^2  {- 代わりにこの両辺を比較すればよい -}
 -}

{- |
線分の交差

>>> s = Seg ((0,0),(6,8))
>>> t = Seg ((0,8),(6,0))
>>> intersect s t
True
>>> s = Seg ((0,0),(2,2))
>>> t = Seg ((0,8),(6,0))
>>> intersect s t
False
>>> s = Seg ((0,0),(3,4))
>>> t = Seg ((0,8),(6,0))
>>> intersect s t
True
 -}
intersect :: (Num a, Ord a) => Seg a -> Seg a -> Bool
intersect (Seg (p1, q1)) (Seg (p2, q2)) =
  cross (line2 p1 q1) p2 q2 &&
  cross (line2 p2 q2) p1 q1

{- |
直線の法線ベクトルを nv 直線上の点を p とする.
2点 q, r を結ぶ線分が、直線と交差する.

>>> cross ( Line ((1, 2), (0, 0)) ) (1, 1) (0, 0)
True
>>> cross ( Line ((1, 2), (0, 0)) ) (1, 1) (-1, -1)
True
>>> cross ( Line ((-2, 1), (0, 0)) ) (-1, 1) (1, -1)
True
>>> cross ( Line ((1, 2), (0, 0)) ) (-19, 11) (21, -9)
False
>>> cross ( Line ((1, 2), (0, 0)) ) (-21, 9) (19, -11)
False
>>> cross ( Line ((1, 2), (0, 0)) ) (-19, 11) (19, -11)
True
>>> cross ( Line ((1, 2), (0, 0)) ) (-21, 9) (21, -9)
True
 -}
cross :: (Num a, Ord a) => Line a -> (a, a) -> (a, a) -> Bool
cross el q r = judgeSide el q * judgeSide el r <= 0
  {-
     d = nv |.| p とすると
     直線の式は nv |.| (x, y) == d
     次の2つの内積値( のコサイン )の符号が、直線のどちら側にあるかで変わることを利用する.
     片方が正、片方が負、なら線分は交差していることになる.
     * nv |.| (p <-> q)
     * nv |.| (p <-> r)
   -}

{- |
直線の法線ベクトルを nv, 直線上の点を p とする.
直線と点q の位置についての判別式を返す.
判別式: nv |.| (p <-> q)

>>> judgeSide ( Line ((1, 2), (-3, 4)) ) (5, 6)  == (1, 2) |.| ((-3, 4) <-> (5, 6))
True
>>> judgeSide ( Line ((1, 2), (-3, 4)) ) (-7, -8)  == (1, 2) |.| ((-3, 4) <-> (-7, -8))
True
 -}
judgeSide :: Num a => Line a -> (a, a) -> a
judgeSide (Line (nv, p)) q = nv |.| (p <-> q)
{-# SPECIALIZE judgeSide :: Line Double -> (Double, Double) -> Double #-}
{-# SPECIALIZE judgeSide :: Line Int -> (Int, Int) -> Int #-}
  {-
     d = nv |.| p とすると
     直線の式は nv |.| (x, y) == d
     次の内積値( のコサイン )の符号が、直線のどちら側にあるかで変わることを利用する.
     nv |.| (p <-> q) == nv |.| p - nv |.| q == d - nv |.| q
   -}

---

{- |
line2 (px, py) (qx, qy) は
2点 (px, py) と (qx, qy) を通る直線の
法線ベクトル nv と直線上の点 (px, py) を返す

>>> line2 (0, 1) (1, 2)
((1,-1),(0,1))
>>> line2 (0, 0) (1, 2)
((2,-1),(0,0))
>>> line2 (0, 0) (2, 1)
((1,-2),(0,0))
>>> line2 (-1, 1) (0, 3)
((2,-1),(-1,1))
 -}
line2 :: Num a => (a, a) -> (a, a) -> Line a
line2 p@(px, py) (qx, qy) = lineAlong v p
  where v = (px - qx, py - qy)
{-# SPECIALIZE line2 :: (Double, Double) -> (Double, Double) -> Line Double #-}
{-# SPECIALIZE line2 :: (Int, Int) -> (Int, Int) -> Line Int #-}

{- |
along v q は
ベクトル v に沿った点 q を通る直線の
法線ベクトル nv と直線上の点 q を返す

>>> lineAlong (1, 1) (1, 2) :: Line Int
((-1,1),(1,2))
>>> lineAlong (1, -2) (1, 2) :: Line Int
((2,1),(1,2))
 -}
lineAlong :: Num a => (a, a) -> (a, a) -> Line a
lineAlong v q = Line (perp v, q)
{-# SPECIALIZE lineAlong :: (Double, Double) -> (Double, Double) -> Line Double #-}
{-# SPECIALIZE lineAlong :: (Int, Int) -> (Int, Int) -> Line Int #-}

---

{- | 2点の間で表現される線分 -}
newtype Seg a = Seg ((a, a), (a, a))

{- | 直線
法線ベクトルと直線上の点のペア (計算に都合が良い)
 -}
newtype Line a = Line ((a, a), (a, a))

{- | 直線の法線ベクトル -}
normal :: Num a => Line a -> (a, a)
normal (Line (n, _)) = n

{- | 直線上のある一点 -}
pointOn :: Num a => Line a -> (a, a)
pointOn (Line (_, p)) = p

instance Show a => Show (Line a) where
  show (Line pair) = show pair

{- | 法線ベクトルは 0 ではダメ
>>> validLine $ Line ((1, 2), (0, 0))
True
>>> validLine $ Line ((0, 0), (0, 0))
False
 -}
validLine :: (Eq a, Num a) => Line a -> Bool
validLine (Line (n, _)) = n /= (0, 0)

{- | 直線が同じ
>>> Line ((1, 2), (0, 0)) == Line ((2, 4), (-2, 1))
True
 -}
instance (Eq a, Num a) => Eq (Line a) where
  Line (n1, p1) == Line (n2, p2)  =
    n1 |*| n2 == 0         &&   {- 法線ベクトルが平行 かつ  -}
    n1 |.| p1 == n1 |.| p2      {- 直線の式 n1 |.| (x, y) = d を p2 が満たす. d == n1 |.| p1 -}

---

{- | 直行するベクトル perpendicular
>>> perp (3, -4) |.| (3, -4)
0
>>> (-3, 4) |.| perp (-3, 4)
0
 -}
perp :: Num a => (a, a) -> (a, a)
perp (px, py) = (-py, px)

infixl 6 <+>, <->
infix 7 |.|, |*|

{- |
>>> (0, 0) <+> (-2, 3) == ( (-2, 3) :: (Int, Int) )
True
>>> (2, -3) <+> (0, 0) == ( (2, -3) :: (Int, Int) )
True
>>> (3, -4) <+> (5, 12) == ( (8, 8) :: (Int, Int) )
True
 -}
(<+>) :: Num a => (a, a) -> (a, a) -> (a, a)
(px, py) <+> (qx, qy) = (px + qx, py + qy)
{-# SPECIALIZE (<+>) :: (Double, Double) -> (Double, Double) -> (Double, Double) #-}
{-# SPECIALIZE (<+>) :: (Int, Int) -> (Int, Int) -> (Int, Int) #-}

{- |
>>> (0, 0) <-> (2, 3) == ( (-2, -3) :: (Int, Int) )
True
>>> (3, 2) <-> (2, 3) == ( (1, -1) :: (Int, Int) )
True
 -}
(<->) :: Num a => (a, a) -> (a, a) -> (a, a)
(px, py) <-> (qx, qy) = (px - qx, py - qy)
{-# SPECIALIZE (<->) :: (Double, Double) -> (Double, Double) -> (Double, Double) #-}
{-# SPECIALIZE (<->) :: (Int, Int) -> (Int, Int) -> (Int, Int) #-}

neg :: Num a => (a, a) -> (a, a)
neg v = (0, 0) <-> v

{- | 内積
>>> (0, 0) |.| (1, 2) :: Int
0
>>> (1, -2) |.| (0, 0) :: Int
0
>>> (3, 4) |.| (-4, 3) :: Int
0
>>> (3, 0) |.| (-5, 5) :: Int
-15
 -}
(|.|) :: Num a => (a, a) -> (a, a) -> a
(px, py) |.| (qx, qy) = px * qx + py * qy
{-# SPECIALIZE (|.|) :: (Double, Double) -> (Double, Double) -> Double #-}
{-# SPECIALIZE (|.|) :: (Int, Int) -> (Int, Int) -> Int #-}

{- | 外積
>>> (0, 0) |*| (1, -2) :: Int
0
>>> (1, 2) |*| (0, 0) :: Int
0
>>> (3, -4) |*| (4, 3) :: Int
25
 -}
(|*|) :: Num a => (a, a) -> (a, a) -> a
(px, py) |*| (qx, qy) = px * qy - py * qx

{- | 長さの二乗
>>> norm2 (0, 0) == (0 :: Int)
True
>>> norm2 (3, 4) == (5 * 5 :: Int)
True
>>> norm2 (-5, 12) == (13 * 13 :: Int)
True
>>> norm2 (8, -15) == (17 * 17 :: Int)
True
>>> norm2 (-7, -24) == (25 * 25 :: Int)
True
 -}
norm2 :: Num a => (a, a) -> a
norm2 v = v |.| v
{-# SPECIALIZE norm2 :: (Double, Double) -> Double #-}
{-# SPECIALIZE norm2 :: (Int, Int) -> Int #-}

{-
 (m^2 - n^2, 2mn, m^2 + n^2)  ←  (m, n)

 (3, 4, 5)     ←  (2, 1)
 (5, 12, 13)   ←  (3, 2)
 (15, 8, 17)   ←  (4, 1)
 (7, 24, 25)   ←  (4, 3)
 -}
