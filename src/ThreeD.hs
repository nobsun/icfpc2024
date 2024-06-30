module ThreeD where

import qualified Data.Set as Set
import Control.Arrow
import Data.Maybe
import Data.Void (Void)

data Direction = L | R | U | D deriving (Show, Eq)
data Arith = Add | Sub | Mul | Quot | Rem | Eq | Neq deriving (Show, Eq)

data Op3D = Move Direction
          | Calc Arith
          | Warp
          | Submit
          | Var Char  -- 'A' and 'B' only
          deriving (Show, Eq)

type Cell = (Int, Int)
type Grid = [(Cell, Place)] -- assoc list, replace HashMap.Strict for performance?
type Tick = Int
type Space = [(Tick, Grid)]

data Place = Operator Op3D
           | Number   Int
           deriving (Show, Eq)

isOperator :: Place -> Bool
isOperator (Operator _) = True
isOperator _            = False

-- 各オペレータの読み取り対象セル
sources :: Op3D -> Cell -> [Cell]
sources (Move L) (x,y) = [(x+1, y  )] -- <
sources (Move R) (x,y) = [(x-1, y  )] -- >
sources (Move U) (x,y) = [(x,   y+1)] -- ^
sources (Move D) (x,y) = [(x,   y-1)] -- v
sources (Calc o) (x,y) = [arg1, arg2] -- +, -, *, /, %, =, #
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
  = [ (c, t) | c <- sources o p, let t = lookup c g]
getSourceCells _ _ = []

-- 各オペレータの書き込み対象セル
-- ただし、これが呼ばれるときは Warpは書き込み対象セルが渡っている前提です
targets :: Op3D -> Cell -> Grid -> [Cell]
targets (Move L) (x,y) g = [(x-1, y  )] -- <
targets (Move R) (x,y) g = [(x+1, y  )] -- >
targets (Move U) (x,y) g = [(x,   y-1)] -- ^
targets (Move D) (x,y) g = [(x,   y+1)] -- v
targets (Calc o) (x,y) g = [arg1, arg2] -- +, -, *, /, %, =, #
  where arg1 = (x+1, y  )
        arg2 = (x,   y+1)
targets Warp     (x,y) g = [(x - vx, y - vy)] -- @
  where dx  = (x-1, y  )
        dy  = (x+1, y  )
        -- 乱暴だけど大丈夫
        Number vx = fromJust $ lookup dx g
        Number vy = fromJust $ lookup dy g
targets _        _     g = [] -- S, Var, Void

getTargetCells :: Grid -> (Cell, Place) -> [(Cell, Place)]
getTargetCells g (p, Operator o)
  = [ (c, t) | c <- targets o p g, let Just t = lookup c g] -- 乱暴だけど大丈夫
getTargetCells _ _ = []

conflict :: [Cell] -> [Cell]
conflict cs = Set.toList $ foldl phi Set.empty cs
  where
    phi :: Set.Set Cell -> Cell -> Set.Set Cell
    phi s c = if Set.member c s then Set.insert c s else s

step :: Grid -> Grid
step g | not (null conflicts) = error "conflict occured"
       | otherwise = writein $ readout g
  where
    readout :: Grid -> Grid
    readout g = undefined
      where
        ro :: [Cell]
        ro = concatMap f ss'
          where f ((_, Operator (Move _)), xs ) = map fst xs
                f ((_, Operator (Calc _)), xs ) = map fst xs
                f ((_, Operator Warp    ), v:_) = [fst v]
                f _                             = []
    
    writein :: Grid -> Grid
    writein g = undefined
    
    ops :: [(Cell, Place)]
    ops = filter (isOperator . snd) g
    ss :: [((Cell, Place), [(Cell, Maybe Place)])]
    ss = map (id &&& getSourceCells g) ops
    ss' :: [((Cell, Place), [(Cell, Place)])]
    ss' = map (second (mapMaybe sequenceA)) ss
    ts :: [((Cell, Place), [(Cell, Place)])]
    ts = map (id &&& getTargetCells g) ops
    conflicts :: [(Cell, Place)]
    conflicts = concatMap snd ts

{- | Grid Layout
  0 1 2
0 . y .
1 x - .
2 . . .
-}
test1 :: Bool
test1 = step s == e
  where
    s = [ ((0,1), Number 5) -- x
        , ((1,0), Number 3) -- y
        , ((1,1), Operator (Calc Sub))
        ]
    e = [ ((1,2), Number 2) -- x-y
        , ((2,1), Number 2) -- x-y
        , ((1,1), Operator (Calc Sub))
        ]
