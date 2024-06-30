module Main where

import Data.List
import Expr

main :: IO ()
main = do
  s <- readFile "answers/efficiency9"
  let e = read s
      cond = findConition e
  putStrLn $ exprToZ3PyExpr cond

findConition :: Expr -> Expr 
findConition = head . f
  where
    f (EIf e _ _) = [e]
    f (EUnary _ e) = f e
    f (EBinary _ e1 e2) = f e1 ++ f e2
    f (ELambda _ e) = f e
    f _ = []

exprToZ3PyExpr :: Expr -> String
exprToZ3PyExpr (EVar n) = "v[" ++ show n ++ "]"
exprToZ3PyExpr (EUnary Not e) = "z3.Not(" ++ exprToZ3PyExpr e ++ ")"
exprToZ3PyExpr e@(EBinary Eql e1 e2) = exprToZ3PyExpr e1 ++ " == " ++ exprToZ3PyExpr e2
exprToZ3PyExpr e@(EBinary And _ _) = "z3.And(" ++ (intercalate ", " (map exprToZ3PyExpr (collectAnd e))) ++ ")"
  where
    collectAnd :: Expr -> [Expr]
    collectAnd (EBinary And e1 e2) = collectAnd e1 ++ collectAnd e2
    collectAnd e = [e]
exprToZ3PyExpr e@(EBinary Or _ _) = "z3.Or(" ++ (intercalate ", " (map exprToZ3PyExpr (collectOr e))) ++ ")"
  where
    collectOr :: Expr -> [Expr]
    collectOr (EBinary Or e1 e2) = collectOr e1 ++ collectOr e2
    collectOr e = [e]
