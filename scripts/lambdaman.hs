{-# LANGUAGE  GHC2021, LambdaCase, MultiWayIf, NPlusKPatterns
            , OverloadedStrings, NoStarIsType, TypeFamilyDependencies
            , DataKinds, PolyKinds, UndecidableInstances
            , ImplicitParams #-}

module Main where

import Data.Array
import Data.Bool
import Data.List
import Data.Maybe
import System.Environment
import System.IO

main :: IO ()
main = do
    { num:sol:_ <- getArgs
    ; let board = "answers/lambdaman" ++ num
    ; let movs  = "solutions/lambdaman" ++ num ++ "/" ++ sol
    ; board <- readFile board
    ; movs <- readFile movs
    ; putStr cls
    ; drawUniv (lines board)
    ; lambdaMan (board, movs)
    }

drawUniv :: [String] -> IO ()
drawUniv ls = sequence_ (zipWith draw [1..] ls)
    where
        draw i s = putStr $ goto (i,1) ++ s ++ "\n"
{- -}
drive :: ([String] -> [String]) -> (String -> String)
drive f = unlines . f . lines

lambdaMan :: (String, String) -> IO ()
lambdaMan = interact . drive . game

game :: (String, String) -> [String] -> [String]
game (b,m) = map (\ g -> output g) . transition . initGame (b,m)

type Univ = Array (Int,Int) Char
type Pos = (Int, Int)
type Dim = (Int, Int)
data Game = Game
    { controls  :: [String]
    , universe  :: Univ
    , dimension :: Dim
    , moves     :: String
    , position  :: Pos
    , output    :: String
    }
{- -}
initGame :: (String, String) -> [String] -> Game
initGame (b,ms) cs = case lines b of
    ls@(l:_) -> case (length ls, length l) of
        (h,w)    -> case fromJust $ findIndex ('L'==) (concat ls) of
            ij       -> case divMod ij w of
                (i,j)    -> Game 
                    { controls  = cs
                    , universe  = listArray ((1,1),(h,w)) (concat ls)
                    , dimension = (h,w)
                    , moves     = ms
                    , position  = (i', j')
                    , output    = goto (i',j')
                               ++ "L" ++ goto (succ h,1)
                    }
                    where (i',j') = (succ i, succ j)

transition :: Game -> [Game]
transition g = g : rests
    where
        rests | final g   = []
              | otherwise = transition (step g)
        
final :: Game -> Bool
final g = or
    [ null $ controls g
    , null $ moves g
    ]

step :: Game -> Game
step g = case controls g of
    c:cs -> case dimension g of
        (h,w) -> case position g of
            (i,j) -> case moves g of
                m:ms -> g
                    { controls = cs
                    , moves    = ms
                    , position = (i',j')
                    , output   = goto (i,j)
                              ++ " "
                              ++ goto (i',j')
                              ++ "L"
                              ++ goto (succ h,1)
                    }
                    where
                        (i',j') = move (universe g) (h,w) m (i,j)

move :: Univ -> Dim -> Char -> Pos -> Pos
move univ (h,w) c (i,j) = case c of
    'L' | 1 < j -> bool (i,j) (i, pred j) (univ ! (i, pred j) /= '#')
    'R' | j < w -> bool (i,j) (i, succ j) (univ ! (i, succ j) /= '#')
    'U' | 1 < i -> bool (i,j) (pred i, j) (univ ! (pred i, j) /= '#')
    'D' | i < h -> bool (i,j) (succ i, j) (univ ! (succ i, j) /= '#')
    _           -> (i,j)
-- -}

goto :: Pos -> String
goto (i,j) = "\ESC[" ++ show i ++ ";" ++ show j ++ "H"

cls :: String
cls = "\ESC[2J"

lambdaman2 :: [String]
lambdaman2 =
    ["L...#."
    ,"#.#.#."
    ,"##...."
    ,"...###"
    ,".##..#"
    ,"....##"
    ]
