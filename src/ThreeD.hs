module ThreeD where

import qualified Data.Set as Set
import qualified Data.HashMap.Strict as Hash
import Data.Hashable
import Control.Arrow
import Data.Maybe
import Data.Void (Void)

data Direction = L | R | U | D deriving (Show, Eq)
data Arith = Add | Sub | Mul | Quot | Rem deriving (Show, Eq)
data Logic = Eql | Neq deriving (Show, Eq)

data Op3D = Move Direction
          | Calc Arith
          | Judge Logic
          | Warp
          -- | Submit -- ??
          deriving (Show, Eq)

calc :: Arith -> Int -> Int -> Int
calc Add  = (+)
calc Sub  = (-)
calc Mul  = (*)
calc Quot = div
calc Rem  = rem

judge :: Logic -> Int -> Int -> Bool
judge Eql = (==)
judge Neq = (/=)

type Cell = (Int, Int)
type Grid = Hash.HashMap Cell Place
type Tick = Int
type Space = [(Tick, Grid)]

data Place = Operator Op3D
           | Number   Int
           | Var Char  -- 'A' and 'B' only
           deriving (Show, Eq)

instance Hashable Place where
  hashWithSalt = defaultHashWithSalt


isOperator :: Place -> Bool
isOperator (Operator _) = True
isOperator _            = False

operators :: Grid -> Hash.HashMap Cell Place
operators = Hash.filter isOperator

-- 各オペレータの読み取り対象セル
sources :: Op3D -> Cell -> [Cell]
sources (Move L) (x,y) = [(x+1, y  )] -- <
sources (Move R) (x,y) = [(x-1, y  )] -- >
sources (Move U) (x,y) = [(x,   y+1)] -- ^
sources (Move D) (x,y) = [(x,   y-1)] -- v
sources (Calc o) (x,y) = [arg1, arg2] -- +, -, *, /, %
  where arg1 = (x-1, y  )
        arg2 = (x,   y-1)
sources (Judge o) (x,y) = [arg1, arg2] -- =, #
  where arg1 = (x-1, y  )
        arg2 = (x,   y-1)
sources Warp     (x,y) = [v,dx,dy,dt] -- @ v は取り出しやすいように先頭に
  where dx = (x-1, y  )
        dy = (x+1, y  )
        dt = (x,   y+1)
        v  = (x,   y-1)
sources _        _     = [] -- S, Var, Void


getSourceCells :: Grid -> (Cell, Place) -> [(Cell, Maybe Place)]
getSourceCells g (p, Operator o)
  = [ (c, t) | c <- sources o p, let t = Hash.lookup c g]
getSourceCells _ _ = []


-- 各オペレータの書き込み対象セル
targets :: Op3D -> Cell -> [(Cell, Place)] -> [(Cell, Place)]
targets (Move L) (x, y) [(_, n)] = [(t, n)]  -- <
  where t = (x-1, y)
targets (Move R) (x, y) [(_, n)] = [(t, n)]  -- >
  where t = (x+1, y)
targets (Move U) (x, y) [(_, n)] = [(t, n)]  -- ^
  where t = (x, y-1)
targets (Move D) (x, y) [(_, n)] = [(t, n)]  -- v
  where t = (x, y+1)
targets (Calc op) (x, y) [(_, Number a), (_, Number b)]
  = [(ret1, v),(ret2, v)]  -- +, -, *, /, %
  where ret1 = (x+1, y)
        ret2 = (x, y+1)
        v = Number $ calc op a b
targets (Judge op) (x, y) [(_, Number a), (_, Number b)]
  = mapMaybe sequenceA [(ret1, v),(ret2, v)]  -- =, #
  where ret1 = (x+1, y)
        ret2 = (x, y+1)
        v = if judge op a b then Just (Number a) else Nothing
targets Warp (x, y) [(_, v), (_, Number dx), (_, Number dy), dt] = [(d, v)] -- @
  where d = (x - dx, y - dy)


-- | 適用可能状態にあるオペレータとその引数情報
--   キー: 適用可能オペレータ
--   値: 引数情報
getApplyableOps :: Grid -> Hash.HashMap (Cell, Place) [(Cell, Place)]
getApplyableOps g = foldr phi Hash.empty ops
  where
    phi :: (Cell, Place) ->
           Hash.HashMap (Cell, Place) [(Cell, Place)] ->
           Hash.HashMap (Cell, Place) [(Cell, Place)]
    phi cell@(c, Operator op) h = case op of
      Move  _ -> Hash.insert cell ss h
      Calc  _ -> undefined
      Judge _ -> undefined
      Warp    -> undefined
      where
        ss :: [(Cell, Place)]
        ss = mapMaybe sequenceA $ getSourceCells g cell
    ops :: [(Cell, Place)]
    ops = Hash.toList $ operators g

-- | TODO: conflict の検出はまだ。Submit があるのでちょっとややこしい
step :: Grid -> Grid
step g = undefined

               
{- | Grid Layout
  0 1 2
0 . y .
1 x - .
2 . . .
-}
sample :: Grid
sample = Hash.fromList [ ((0,1), Number 5) -- x
                       , ((1,0), Number 3) -- y
                       , ((1,1), Operator (Calc Sub))
                       ]
