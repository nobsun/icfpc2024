module ThreeD where

import Data.Char (ord, chr)
import Data.List
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
           | Submit
           | Var Char  -- 'A' and 'B' only
           deriving (Show, Eq)

instance Hashable Place where
  hashWithSalt = defaultHashWithSalt


isOperator :: Place -> Bool
isOperator (Operator _) = True
isOperator _            = False

operators :: Grid -> Hash.HashMap Cell Place
operators = Hash.filter isOperator

isSubmit :: Place -> Bool
isSubmit Submit = True
isSubmit _      = False

submits :: Grid -> Set.Set Cell
submits = Set.fromList . Hash.keys . Hash.filter isSubmit

isVar :: Place -> Bool
isVar (Var _) = True
isVar _       = False

vars :: Grid -> [(Char, Cell)]
vars g = map (f . swap) $ Hash.toList vs
  where
    vs = Hash.filter isVar g
    swap (a, b) = (b, a)
    f (Var v, c) = (v, c)
    f _ = error "vars: impossible"

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


getSourceCells :: Grid -> (Cell, Place) -> [(Cell, Place)]
getSourceCells g (p, Operator o)
  = mapMaybe sequenceA [ (c, t) | c <- sources o p, let t = Hash.lookup c g]
getSourceCells _ _ = []


-- 各オペレータの書き込み対象セル
targets :: Place -> Cell -> [(Cell, Place)] -> [(Cell, Place)]
targets (Operator (Move L)) (x, y) [(_, n)] = [(t, n)]  -- <
  where t = (x-1, y)
targets (Operator (Move R)) (x, y) [(_, n)] = [(t, n)]  -- >
  where t = (x+1, y)
targets (Operator (Move U)) (x, y) [(_, n)] = [(t, n)]  -- ^
  where t = (x, y-1)
targets (Operator (Move D)) (x, y) [(_, n)] = [(t, n)]  -- v
  where t = (x, y+1)
targets (Operator (Calc op)) (x, y) [(_, Number a), (_, Number b)]
  = [(ret1, v),(ret2, v)]  -- +, -, *, /, %
  where ret1 = (x+1, y)
        ret2 = (x, y+1)
        v = Number $ calc op a b
targets (Operator (Judge op)) (x, y) [(_, Number a), (_, Number b)]
  = mapMaybe sequenceA [(ret1, v),(ret2, v)]  -- =, #
  where ret1 = (x+1, y)
        ret2 = (x, y+1)
        v = if judge op a b then Just (Number a) else Nothing
targets (Operator Warp) (x, y) [(_, v), (_, Number dx), (_, Number dy), dt]
  = [(d, v)] -- @
  where d = (x - dx, y - dy)
targets _ _ _ = [] -- S, Var, Void


data Updatable = Del (Cell, Place)
               | Upd (Cell, Place)
               | Ban (Cell, Place)
               deriving (Show, Eq)

updatables :: Set.Set Cell -> Grid -> [Updatable]
updatables bombs g = foldr phi [] ops
  where
    phi :: (Cell, Place) -> [Updatable] -> [Updatable]
    phi cell@(c, p@(Operator _)) acc = xs ++ acc
      where
        xs :: [Updatable]
        xs = (Del <$> ss) <> (Upd <$> ts)
        ss :: [(Cell, Place)]
        ss = getSourceCells g cell
        ts :: [(Cell, Place)]
        ts = targets p c ss
    ops :: [(Cell, Place)]
    ops = Hash.toList $ operators g

-- | TODO: conflict の検出はまだ。Submit があるのでちょっとややこしい
step :: Set.Set Cell -> Grid -> (Maybe Place, Grid)
step bombs g = foldr phi (Nothing, g) upd
  where
    phi :: Updatable -> (Maybe Place, Grid) -> (Maybe Place, Grid)
    phi (Del (c, _)) (b, g') = (b, Hash.delete c g')
    phi (Upd (c, p)) (b, g') = (b, Hash.insert c p g')
    phi (Ban (c, _)) (b, g') = (b, Hash.delete c g')
    
    upd = updatables bombs g

run :: Grid -> Space
run g = unfoldr psi (0, g)
  where
    psi :: (Tick, Grid) -> Maybe ((Tick, Grid), (Tick, Grid))
    psi (t, g) | isNothing boom = Just ((t, g), (t+1, g'))
               | otherwise   = Nothing

    (boom, g') = step bombs g

    bombs :: Set.Set Cell
    bombs = submits g

initBy :: [(Char, Int)] -> Grid -> Grid
initBy vals g = g'
  where
    g' = foldr phi g vals
      where
        phi :: (Char, Int) -> Grid -> Grid
        phi (v, n) = Hash.insert c (Number n)
          where
            Just c = lookup v vs

        vs = vars g

runWith :: [(Char, Int)] -> Grid -> Space
runWith vals g = run $ initBy vals g

showGame :: (Int, Int) -> Grid -> String
showGame (w, h) g = unlines $ map (intersperse ' ') $ grid
  where
    grid = [[toChar c
            | x <- [0..w-1]
            , let c = Hash.lookup (x, y) g]
           | y <- [0..h-1]]
    toChar :: Maybe Place -> Char
    toChar Nothing = '.'
    toChar (Just (Number n)) = chr $ ord '0' + n
    toChar (Just (Operator (Move L)))    = '<'
    toChar (Just (Operator (Move R)))    = '>'
    toChar (Just (Operator (Move U)))    = '^'
    toChar (Just (Operator (Move D)))    = 'v'
    toChar (Just (Operator (Calc Add)))  = '+'
    toChar (Just (Operator (Calc Sub)))  = '-'
    toChar (Just (Operator (Calc Mul)))  = '*'
    toChar (Just (Operator (Calc Quot))) = '/'
    toChar (Just (Operator (Calc Rem)))  = '%'
    toChar (Just (Operator (Judge Eql))) = '='
    toChar (Just (Operator (Judge Neq))) = '#'
    toChar (Just (Operator Warp))        = '@'
    toChar (Just Submit)                 = 'S'
    toChar (Just (Var v))                = v

    

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

--   0 1 2 3 4 5 6 7 8
-- 0 . . . . 0 . . . .
-- 1 . B > . = . . . .
-- 2 . v 1 . . > . . .
-- 3 . . - . . . + S .
-- 4 . . . . . ^ . . .
-- 5 . . v . . 0 > . .
-- 6 . . . . . . A + .
-- 7 . 1 @ 6 . . < . .
-- 8 . . 3 . 0 @ 3 . .
-- 9 . . . . . 3 . . .
--
game :: Grid
game = Hash.fromList [ ((4,0), Number 0)
                     , ((1,1), Var 'B')
                     , ((2,1), Operator (Move R))
                     , ((4,1), Operator (Judge Eql))
                     , ((1,2), Operator (Move D))
                     , ((2,2), Number 1)
                     , ((5,2), Operator (Move R))
                     , ((2,3), Operator (Calc Sub))
                     , ((6,3), Operator (Calc Add))
                     , ((7,3), Submit)
                     , ((5,4), Operator (Move U))
                     , ((2,5), Operator (Move D))
                     , ((5,5), Number 0)
                     , ((6,5), Operator (Move R))
                     , ((6,6), Var 'A')
                     , ((7,6), Operator (Calc Add))
                     , ((1,7), Number 1)
                     , ((2,7), Operator Warp)
                     , ((3,7), Number 6)
                     , ((6,7), Operator (Move L))
                     , ((2,8), Number 3)
                     , ((4,8), Number 0)
                     , ((5,8), Operator Warp)
                     , ((6,8), Number 3)
                     , ((5,9), Number 3)
                     ]
