{-# LANGUAGE GHC2021 #-}
module ThreeD where

import Control.Arrow (second)
import Control.Monad (forM_)
import Data.Char (ord, chr)
import Data.Function (on)
import qualified Data.HashMap.Strict as Hash
import Data.List -- (foldl', foldr, find, partition, unfoldr)
import qualified Data.Map as Map
import Data.Maybe (fromJust, mapMaybe, isJust, isNothing)
import qualified Data.Set as Set

data Direction = L | R | U | D deriving (Show, Eq)
data Arith = Add | Sub | Mul | Quot | Rem deriving (Show, Eq)
data Logic = Eql | Neq deriving (Show, Eq)

data Op3D = Move  !Direction
          | Calc  !Arith
          | Judge !Logic
          | Warp
          deriving (Show, Eq)

calc :: Arith -> Place -> Place -> Place
calc Add  (Number x) (Number y) = Number (x+y)
calc Sub  (Number x) (Number y) = Number (x-y)
calc Mul  (Number x) (Number y) = Number (x*y)
calc Quot (Number x) (Number y) = Number (x `quot` y)
calc Rem  (Number x) (Number y) = Number (x `rem` y)
calc op   x          y          = error $ "unexpected arguments: " ++ show op ++ " for " ++ show (x, y)

judge :: Logic -> Place -> Place -> Bool
judge Eql (Number x) (Number y) = x == y
judge Neq (Number x) (Number y) = x /= y
judge op x y = error $ "judge: impossible: " ++ show (op, x, y)

type Cell = (Int, Int)
type Grid = Hash.HashMap Cell Place
type Tick = Int
type Space = [Grid]

data Place = Operator !Op3D
           | Number   !Int
           | Submit
           | Var !Char  -- 'A' and 'B' only
           deriving (Show, Eq)

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

data Update = Erase ![(Cell, Place)]
            | Write ![(Cell, Place)]
            | TimeWarp !Tick !(Cell, Place)
            deriving (Show, Eq)

-- 各オペレータの更新動作アクション
operate :: Grid -> (Cell, Place) -> [Update]
operate g ((x, y), Operator (Move L)) = maybe [] f q  -- <
  where q = Hash.lookup (x+1, y) g
        f r = [ Erase [((x+1, y), r)], Write [((x-1, y), r)]]
operate g ((x, y), Operator (Move R)) = maybe [] f q  -- >
  where q = Hash.lookup (x-1, y) g
        f r = [ Erase [((x-1, y), r)], Write [((x+1, y), r)]]
operate g ((x, y), Operator (Move U)) = maybe [] f q  -- ^
  where q = Hash.lookup (x, y+1) g
        f r = [ Erase [((x, y+1), r)], Write [((x, y-1), r)]]
operate g ((x, y), Operator (Move D)) = maybe [] f q  -- v
  where q = Hash.lookup (x, y-1) g
        f r = [ Erase [((x, y-1), r)], Write [((x, y+1), r)]]
operate g ((x, y), Operator (Calc op)) = maybe [] f r -- +, -, *, /, %
  where p = Hash.lookup (x-1, y) g
        q = Hash.lookup (x, y-1) g
        r = do { p' <- p; q' <- q; return (p', q', calc op p' q') }
        f (a, b, c) = [ Erase [((x-1, y), a), ((x, y-1), b)]
              , Write [((x+1, y), c), ((x, y+1), c)]]
operate g ((x, y), Operator (Judge op)) = maybe [] f r -- =, #
  where p = Hash.lookup (x-1, y) g
        q = Hash.lookup (x, y-1) g
        r = do { p' <- p; q' <- q; return (judge op p' q', p', q') }
        f (c, a, b)
          | c = [ Erase [((x-1, y), a), ((x, y-1), b)]
                , Write [((x+1, y), a), ((x, y+1), b)]]
          | otherwise = []
operate g ((x, y), Operator Warp) = maybe [] f dr -- @
  where dx = Hash.lookup (x-1, y) g
        dy = Hash.lookup (x+1, y) g
        dt = Hash.lookup (x, y+1) g
        dv = Hash.lookup (x, y-1) g
        dr :: Maybe (Cell, Tick, Place)
        dr = do { Number dx' <- dx
                ; Number dy' <- dy
                ; Number dt' <- dt
                ; dv' <- dv
                ; return ((x - dx', y - dy'), dt', dv')
                }
        f (c, t, v) = [TimeWarp t (c, v)]
operate _ (c, op) = error $ "unexpected operator: " ++ show op ++ " at " ++ show c

initBy :: [(Char, Int)] -> Grid -> Grid
initBy vals g = g'
  where
    g' = foldr phi g vals
      where
        phi :: (Char, Int) -> Grid -> Grid
        phi (v, n) h = foldr (f . snd) h cs
          where
            f c = Hash.insert c (Number n)
            cs = filter ((==v) . fst) vs

        vs = vars g

-- | ステップの無限列を生成するので停止させるのは外でやる (fst が Just になったら止める)
steps :: Grid -> [(Maybe Int, Grid)]
steps initGrid = start:unfoldr psi [start]
  where
    start = (Nothing, initGrid)
    psi :: [(Maybe Int, Grid)] -> Maybe ((Maybe Int, Grid), [(Maybe Int, Grid)])
    psi [] = error "unreachable!"
    psi hist@((_, g):_)
      | not (null submitConflicts) = error $ "submit conflict occured: " ++ show submitConflicts
      | not (null writeConflicts)  = error $ "write conflict occured: " ++ show writeConflicts
      | isJust done                = let r = (retVal, g') in Just (r, r:hist)
      | not (null warps)           = let r = (Nothing, timewarp) in Just (r, r:hist)
      | otherwise                  = let r = (retVal, g') in Just (r, r:hist)
      where
        g' = foldr phi g upds
          where
            phi :: Update -> Grid -> Grid
            phi (Erase cs) h = foldr (Hash.delete . fst) h cs
            phi (Write cs) h = foldr (uncurry Hash.insert) h cs
            phi (TimeWarp _ _) h = h -- NOTE: submit の時には何もしない

        ops :: [(Cell, Place)]
        ops = Hash.toList $ operators g

        upds :: [Update]
        upds = concatMap (operate g) ops

        wrs :: [Update]
        wrs = filter f upds
          where
            f (Write _) = True
            f _         = False

        sbmts :: [(Cell, Place)]
        sbmts = filter (isSubmit . snd) $ Hash.toList g

        conflicts :: [(Cell, Place)]
        conflicts = concatMap f wrs
          where
            f (Write cs) = cs
            f u          = error $ "unexpected update: " ++ show u

        (submitConflicts, writeConflicts) = (ss', ws')
          where
            groupBy' :: forall a k. (Eq k, Ord k) => (a -> k) -> [a] -> [[a]]
            groupBy' f = Map.elems . foldr phi Map.empty
              where
                phi :: a -> Map.Map k [a] -> Map.Map k [a]
                phi x = Map.insertWith (<>) (f x) [x]
            
            ss, ws :: [(Cell, Place)]
            (ss, ws) = partition (\(c, _) -> c `elem` map fst sbmts) conflicts
            -- 同一の Submit Cell に対する Write は同じ値でなければ Conflict
            ss' = filter (\xs -> not . and $ zipWith (/=) xs (tail xs)) $ groupBy' fst ss
            -- Submit Cell 以外は同一の Cell に対する Write は複数あったら Conflict
            ws' = filter (\xs -> length xs > 1) $ groupBy' fst ws

        done :: Maybe Update
        done = find f wrs
          where
            f (Write cs) = any (\(c, _) -> c `elem` map fst sbmts) cs
            f _          = False

        retVal :: Maybe Int
        retVal = do
          Write ((_, Number v):_) <- done
          return v

        warps :: [Update]
        warps = filter isWarp upds
          where
            isWarp (TimeWarp _ _) = True
            isWarp _              = False

        -- NOTE: Timewarp がある場合は全て同じ Tick に戻るはず
        timewarp = foldr phi (snd (hist !! t)) warps
          where
            phi (TimeWarp _ (c, p)) = Hash.insert c p
            phi op = error $ "unexpected warp action: " ++ show op
            t = case head warps of
              TimeWarp tick _ -> tick
              op              -> error $ "unexpected warp action: " ++ show op


-- | c.f.) solveProblem "3d2/sol1.txt" [('A', 3),('B',2)]
solveProblem :: String -> [(Char, Int)] -> IO ()
solveProblem prob params = do
  g <- readProblem prob
  runAndDrawWith' params g


runAndDrawWith' :: [(Char, Int)] -> Grid -> IO ()
runAndDrawWith' vals g = runAndDrawWith (w+2, h+2) vals g
  where w = maximum $ map fst g'
        h = maximum $ map snd g'
        g' = Hash.keys g


runAndDrawWith :: (Int, Int) -> [(Char, Int)] -> Grid -> IO ()
runAndDrawWith wh vals g = do
  forM_ (zip [1::Int ..] gs) $ \(t, (v, g')) -> do
    putStrLn $ "Step " ++ show t ++ ":"
    drawGame wh g'
    putStrLn ""
    maybe (putStr "") (\val -> putStrLn $ "Result: " ++ show val) v
  where gs = runWith vals g

runWith :: [(Char, Int)] -> Grid -> [(Maybe Int, Grid)]
runWith vals g = run $ initBy vals g

run :: Grid -> [(Maybe Int, Grid)]
run = snoc . second head . break (isJust . fst) . steps
  where
    snoc :: ([a], a) -> [a]
    snoc (xs, x) = xs ++ [x]


drawGame :: (Int, Int) -> Grid -> IO ()
drawGame (w, h) g = putStrLn $ showGame (w, h) g

showGame :: (Int, Int) -> Grid -> String
showGame (w, h) g = unlines $ map (concatMap pad) grid
  where
    pad s = case length s of
      1 -> ' ' : s
      2 -> s
      _ -> "??"
    
    grid = [[toStr c
            | x <- [0..w-1]
            , let c = Hash.lookup (x, y) g]
           | y <- [0..h-1]]
    toStr :: Maybe Place -> String
    toStr Nothing                       = "."
    toStr (Just (Number n))             = show n
    toStr (Just (Operator (Move L)))    = "<"
    toStr (Just (Operator (Move R)))    = ">"
    toStr (Just (Operator (Move U)))    = "^"
    toStr (Just (Operator (Move D)))    = "v"
    toStr (Just (Operator (Calc Add)))  = "+"
    toStr (Just (Operator (Calc Sub)))  = "-"
    toStr (Just (Operator (Calc Mul)))  = "*"
    toStr (Just (Operator (Calc Quot))) = "/"
    toStr (Just (Operator (Calc Rem)))  = "%"
    toStr (Just (Operator (Judge Eql))) = "="
    toStr (Just (Operator (Judge Neq))) = "#"
    toStr (Just (Operator Warp))        = "@"
    toStr (Just Submit)                 = "S"
    toStr (Just (Var v))                = [v]


readProblem :: String -> IO Grid
readProblem prob = do
  f <- readFile $ "solutions/" <> prob
  let ls = map (readLine . words) $ lines f
  let ret = zipWith (\y xs -> map (\(x,c) -> ((x,y),c)) xs) [0..] $ map (zip [0..]) ls
  return $ Hash.fromList $ concatMap (mapMaybe sequenceA) ret
  where
    readLine :: [String] -> [Maybe Place]
    readLine = map readPlace
    
    readPlace :: String -> Maybe Place
    readPlace "." = Nothing
    readPlace "<" = Just (Operator (Move L))
    readPlace ">" = Just (Operator (Move R))
    readPlace "^" = Just (Operator (Move U))
    readPlace "v" = Just (Operator (Move D))
    readPlace "+" = Just (Operator (Calc Add))
    readPlace "-" = Just (Operator (Calc Sub))
    readPlace "*" = Just (Operator (Calc Mul))
    readPlace "/" = Just (Operator (Calc Quot))
    readPlace "%" = Just (Operator (Calc Rem))
    readPlace "=" = Just (Operator (Judge Eql))
    readPlace "#" = Just (Operator (Judge Neq))
    readPlace "@" = Just (Operator Warp)
    readPlace "S" = Just Submit
    readPlace "A" = Just (Var 'A')
    readPlace "B" = Just (Var 'B')
    readPlace s   = Just (Number (read s))
    

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

{- | Grid Layout
  0 1 2 3 4 5 6 7 8
0 . . . . B . . . .
1 . 0 > . = . . . .
2 . v 1 . . > . . .
3 . . + . . . + S .
4 . . . . . ^ . . .
5 . . v . . A > . .
6 . . . . . . 0 + .
7 . 1 @ 6 . . < . .
8 . . 3 . 0 @ 3 . .
9 . . . . . 3 . . .
-}
add :: Grid
add = Hash.fromList [ ((4,0), Var 'B')
                    , ((1,1), Number 0)
                    , ((2,1), Operator (Move R))
                    , ((4,1), Operator (Judge Eql))
                    , ((1,2), Operator (Move D))
                    , ((2,2), Number 1)
                    , ((5,2), Operator (Move R))
                    , ((2,3), Operator (Calc Add))
                    , ((6,3), Operator (Calc Add))
                    , ((7,3), Submit)
                    , ((5,4), Operator (Move U))
                    , ((2,5), Operator (Move D))
                    , ((5,5), Var 'A')
                    , ((6,5), Operator (Move R))
                    , ((6,6), Number 0)
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

{- | Grid Layout
  0 1 2 3 4 5 6 7 8
0 . . . . 0 . . . .
1 . B > . = . . . .
2 . v 1 . . > . . .
3 . . - . . . + S .
4 . . . . . ^ . . .
5 . . v . . 0 > . .
6 . . . . . . A + .
7 . 1 @ 6 . . < . .
8 . . 3 . 0 @ 3 . .
9 . . . . . 3 . . .
-}
mul :: Grid
mul = Hash.fromList [ ((4,0), Number 0)
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

{- | Grid Layout
  0 1 2 3 4 5 6 7 8
0 . . . . 0 . . . .
1 . B > . = . . . .
2 . v 1 . . > . . .
3 . . - . . . + S .
4 . . . . . ^ . . .
5 . . v . . 1 > . .
6 . . . . . . A * .
7 . 1 @ 6 . . < . .
8 . . 3 . 0 @ 3 . .
9 . . . . . 3 . . .
-}
expr :: Grid
expr = Hash.fromList [ ((4,0), Number 0)
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
                     , ((5,5), Number 1)
                     , ((6,5), Operator (Move R))
                     , ((6,6), Var 'A')
                     , ((7,6), Operator (Calc Mul))
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

