module LambdaMan
  ( solve
  , solveFile
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

solve :: String -> String
solve = solveGreedy . mkStage

solveFile :: FilePath -> IO String
solveFile fname = solve <$> readFile fname

type Pos = (Int, Int)

-- size
-- position of walls
-- position of pills
-- position of lambda-man
type Stage = ((Int, Int), Set Pos, Set Pos, Pos)

mkStage :: String -> Stage
mkStage s =
  ( (length (head ls), length ls)
  , Map.keysSet $ Map.filter ('#' ==) m
  , Map.keysSet $ Map.filter ('.' ==) m
  , head [(i,j) | ((i,j), c) <- Map.toList m, c == 'L']
  )
  where
    ls = [l | l <- lines s, not (null l)]
    m = Map.fromList [((x,y), c) | (y, l) <- zip [0..] ls, (x, c) <- zip [0..] l]

solveGreedy :: Stage -> String
solveGreedy ((w, h), walls, pills0, p0) = f pills0 p0 []
  where
    f pills p hist
      | Set.null pills = reverse hist
      | otherwise =
          case findNearestPill ((w,h)) walls pills p of
            Nothing -> undefined
            Just (p', moves) -> f (Set.delete p' pills) p' (moves ++ hist)

findNearestPill :: (Int, Int) -> Set Pos -> Set Pos -> Pos -> Maybe (Pos, String)
findNearestPill (w, h) walls pills p0
  | Set.null pills = Nothing
  | otherwise = f Map.empty (Map.singleton p0 [])
  where
    walls' = Map.fromSet (const ()) walls

    f :: Map Pos () -> Map Pos String -> Maybe (Pos, String)
    f visited new
      | Map.null new = Nothing
      | Map.null tmp = f visited' new'
      | otherwise = Just (head (Map.toList tmp))
      where
        tmp = new `Map.intersection` Map.fromSet (const ()) pills
        visited' = visited `Map.union` Map.map (const ()) new
        new' =
          (Map.fromList $ concat $
           [ [((x-1, y), 'L':hist) | 0 < x] ++
             [((x+1, y), 'R':hist) | x < w-1] ++
             [((x, y-1), 'U':hist) | 0 < h] ++
             [((x, y+1), 'D':hist) | y < h-1]
           | ((x,y),hist) <- Map.toList new
           ])
          `Map.difference` visited
          `Map.difference` walls'
