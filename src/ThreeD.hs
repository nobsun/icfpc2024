module ThreeD where

-- import qualified Data.HashMap.Strict as HM
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

getTargets :: Op3D -> Cell -> [Cell]
getTargets (Move L) (x,y) = [(x+1, y  )] -- <
getTargets (Move R) (x,y) = [(x-1, y  )] -- >
getTargets (Move U) (x,y) = [(x,   y+1)] -- ^
getTargets (Move D) (x,y) = [(x,   y-1)] -- v
getTargets (Calc o) (x,y) = [arg1, arg2] -- +, -, *, /, %, =, #
  where arg1 = (x-1, y  )
        arg2 = (x,   y-1)
getTargets Warp     (x,y) = [dx,dy,dt,v] -- @
  where dx = (x-1, y  )
        dy = (x+1, y  )
        dt = (x,   y+1)
        v  = (x,   y-1)
getTargets _        _     = [] -- S, Var, Void

step :: Grid -> Grid
step g = undefined

{- | Grid Layout
  0 1 2
0 . y .
1 x - .
2 . . .
-}
test1 :: Bool
test1 = step s == e
  where s = [ ((0,1), Number 5) -- x
            , ((1,0), Number 3) -- y
            , ((1,1), Operator (Calc Sub))
            ]
        e = [ ((1,2), Number 2) -- x-y
            , ((2,1), Number 2) -- x-y
            , ((1,1), Operator (Calc Sub))
            ]
          

