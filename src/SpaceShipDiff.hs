module SpaceShipDiff where

import qualified Data.ByteString.Char8 as B8
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import System.Directory
import System.FilePath

import Imports
import Vec2
import Expr (Expr' (EStr))
import qualified CommunicateIO as Com

solveNaiveAnProb :: FilePath -> IO ()
solveNaiveAnProb name = do
  putStrLn $ "solve-naive: " ++ name
  ex <- doesFileExist ("answers" </> name)
  when (not ex) $ putStrLn $ "not exist: " ++ name
  when ex $ do
    ps <- parseAnProb name
    let failure _ = pure ()
        success buttons = print =<< Com.solve name (EStr $ B8.pack buttons)
    either failure success $ solveNaive ps

parseAnProb :: FilePath -> IO [Pos]
parseAnProb name = parseInput =<< readFile ("answers" </> name)


---

solveNaive :: [Pos] -> Either [Acc] String
solveNaive = accNaiveButtons . diffAccList

---

type Pos = (Int, Int)
type Acc = (Int, Int)

diffAccList :: [Pos] -> [Acc]
diffAccList ps = as
  where vs = zipWith (<->) ps ((0,0) : ps)
        as = zipWith (<->) vs ((0,0) : vs)

accBound :: Acc -> Bool
accBound (ax, ay) = -1 <= ax && ax <= 1 && -1 <= ay && ay <= 1

accNaiveButtons :: [Acc] -> Either [Acc] String
accNaiveButtons as
  | all accBound as  = Right [buttonMap ! a | a <- as]
  | otherwise        = Left [a | a <- as, not (accBound a)]

---

{-
   7  8  9
   4  5  6
   1  2  3
 -}
buttonMap :: Map Acc Char
buttonMap =
  Map.fromList
  [ ((-1, -1),  '1')
  , (( 0, -1),  '2')
  , (( 1, -1),  '3')
  --
  , ((-1,  0),  '4')
  , (( 0,  0),  '5')
  , (( 1,  0),  '6')
  --
  , ((-1,  1),  '7')
  , (( 0,  1),  '8')
  , (( 1,  1),  '9')
  ]

---

parseInput :: String -> IO [Pos]
parseInput = mapM parseLine . lines

parseLine :: String -> IO Pos
parseLine s = do
  xy <- mapM readIO (words s)
  case xy of
    [x, y] -> pure (x, y)
    _      -> fail $ "SpaceShipDiff.parseLine: parse error: " ++ show xy
