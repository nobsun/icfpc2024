{-# LANGUAGE  GHC2021, LambdaCase, MultiWayIf, NPlusKPatterns
            , OverloadedStrings, NoStarIsType, TypeFamilyDependencies
            , DataKinds, PolyKinds, UndecidableInstances
            , ImplicitParams #-}

module Main where

import Data.Array
import Data.List
import Data.Maybe
import System.Environment
import System.IO

main :: IO ()
main = do
    { bd:num:_ <- getArgs
    ; let board = "answers/" ++ bd ++ num
    ; let movs  = "solutions/" ++ bd ++ num ++"/sol.txt"
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
                    , universe  = listArray ((1,1),(h,w)) (concat cs)
                    , dimension = (h,w)
                    , moves     = ms
                    , position  = (i', j')
                    , output    = goto (i',j') ++ "L"
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
                    , output   = goto (i,j) ++ " " ++ goto (i',j') ++ "L"
                    }
                    where
                        (i',j') = move (h,w) m (i,j)

move :: Dim -> Char -> Pos -> Pos
move (h,w) c (i,j) = case c of
    'L' | 1 < j -> (i, pred j)
    'R' | j < w -> (i, succ j)
    'U' | 1 < i -> (pred i, j)
    'D' | i < w -> (succ i, j)
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

