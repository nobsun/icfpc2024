module Compress
  ( compress
  ) where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Expr


compress :: ByteString -> Expr
compress s
  | BS.null s = EStr BS.empty
  | otherwise =
      foldr (\(v, def) body -> EBinary Apply (ELambda v body) def)
            body
            bindings
  where
    bindings = [(v, def) | cdict <- Map.elems dict, (v, def) <- IntMap.elems cdict]
    body = foldr1 (EBinary Concat) [EVar (fst (dict Map.! BS.head g IntMap.! BS.length g)) | g <- gs]

    gs :: [ByteString]
    gs = BS.group s

    usage :: Map Char IntSet
    usage = Map.fromListWith IntSet.union [(BS.head g, IntSet.singleton (BS.length g)) | g <- gs]

    dict :: Map Char (IntMap (Var, Expr))
    (_, dict) = Map.foldlWithKey f (0, Map.empty) (Map.map saturate usage)
      where
        f :: (Var, Map Char (IntMap (Var, Expr))) -> Char -> IntSet -> (Var, Map Char (IntMap (Var, Expr)))
        f (n, dict) c xs =
          case g c (IntSet.toList xs) (n, IntMap.empty) of
            (n', cdict) -> (n', Map.insert c cdict dict)

        g :: Char -> [Int] -> (Var, IntMap (Var, Expr)) -> (Var, IntMap (Var, Expr))
        g _ [] (n, cdict) = (n, cdict)
        g c (x : xs) (n, cdict)
          | x `IntMap.member` cdict = g c xs (n, cdict)
          | otherwise =
              case IntMap.lookupMax cdict of
                Nothing -> g c xs (n+1, IntMap.insert x (n, EStr (BS.replicate x c)) cdict)
                Just (x1, (v1, _)) ->
                  let x2 = x - x1
                      (v2, _) = cdict IntMap.! x2
                   in g c xs (n+1, IntMap.insert x (n, EBinary Concat (EVar v1) (EVar v2)) cdict)


saturate :: IntSet -> IntSet
saturate xs
  | IntSet.null xs = xs
  | IntSet.null (ds IntSet.\\ xs) = xs
  | otherwise = saturate (xs `IntSet.union` ds)
  where
    xs' = IntSet.toAscList xs
    ds = IntSet.fromList (zipWith (-) (tail xs') xs')
