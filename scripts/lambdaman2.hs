{-# LANGUAGE  GHC2021, LambdaCase, MultiWayIf, NPlusKPatterns
            , OverloadedStrings, NoStarIsType, TypeFamilyDependencies
            , DataKinds, PolyKinds, UndecidableInstances
            , ImplicitParams #-}

module Main where

import Data.Array
import Data.Bool
import Data.Char
import Data.List
import Data.Maybe
import System.Environment
import System.IO
import Debug.Trace

type Dim = (Int, Int)
type Pos = (Int, Int)
type Board = Array Pos Char

main :: IO ()
main = do
    { hSetEcho stdin False
    ; hSetBuffering stdin NoBuffering
    ; hSetBuffering stdout NoBuffering
    ; args <- getArgs
    ; case args of
        [num] -> do
            { let boardFile = "answers/lambdaman" ++ num
            ; let solutionFile = "solutions/lambdaman" ++ num ++ "/solv.txt"
            ; board <- readFile boardFile
            ; putStr cls
            ; drawUniv (lines board)
            ; let dim = dimension board
            ; let board' = concat (lines board)
            ; let pos = initialPos dim board'
            ; let bd  = listArray ((1,1), dim) board'
            ; hdl <- openFile solutionFile WriteMode
            ; hSetBuffering hdl NoBuffering
            ; hPutStr stdout $ indicate (fst dim) pos
            ; loop bd dim pos hdl
            }
    }

drawUniv :: [String] -> IO ()
drawUniv ls = sequence_ (zipWith draw [1..] ls)
    where
        draw i s = putStr $ concat [goto (i,1), concatMap conv s, "\n"]
        conv = \ case
            '#' -> wall
            '.' -> tablet
            ' ' -> blank
            'L' -> lambda

dimension :: String -> Dim
dimension b = let
        ls = lines b
    in (length ls, length (head ls))

initialPos :: Dim -> String -> Pos
initialPos (h,w) b
    = case fromJust $ elemIndex 'L' b of
        ij -> case divMod ij w of
            (i,j) -> (succ i, succ j)

loop :: Board -> Dim -> Pos -> Handle -> IO ()
loop bd (h,w) (i,j) hdl = do
    { eof <- hIsEOF stdin
    ; if eof then hClose hdl >> return ()
      else do
        { c' <- hGetChar stdin
        ; let c = toUpper c'
        ; case move bd (h,w) c (i,j) of
            (i',j') -> do
                { if (i,j) == (i',j') then do
                    { hPutStr stdout (indicate h (i,j))
                    ; loop bd (h,w) (i,j) hdl
                    }
                  else do
                    { hPutStr stdout $ goto (i,j) ++ blank ++ goto (i',j') ++ lambda
                    ; hPutChar hdl c
                    ; hPutStr stdout (indicate h (i',j'))
                    ; loop bd (h,w) (i',j') hdl
                    }
                }         
        }
    }

indicate :: Int -> Pos -> String
indicate h pos = goto (h+2, 1) ++ "\ESC[0J" ++ show pos

move :: Board -> Dim -> Char -> Pos -> Pos
move b (h,w) c (i,j) = case c of
    'L' | 1 < j -> bool (i, j')  (i,j) (b ! (i, j')  == '#')
    'R' | j < w -> bool (i, j'') (i,j) (b ! (i, j'') == '#')
    'U' | 1 < i -> bool (i', j)  (i,j) (b ! (i', j)  == '#')
    'D' | i < h -> bool (i'', j) (i,j) (b ! (i'', j) == '#')
    _           -> (i,j)
    where (i',j')    = (pred i, pred j)
          (i'', j'') = (succ i, succ j)

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

blank, tablet, wall, lambda :: String
blank  = "\ESC[47m \ESC[0m"
tablet = "\ESC[46m.\ESC[0m"
wall   = "\ESC[40m \ESC[0m"
lambda = "\ESC[43mL\ESC[0m"

