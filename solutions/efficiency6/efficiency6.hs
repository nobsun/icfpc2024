import Control.Monad
import Data.Numbers.Primes -- https://hackage.haskell.org/package/primes

fib :: [Integer]
fib = 1 : 1 : zipWith (+) fib (tail fib)

main = print $ head $ [n | (n, x) <- zip [0..] fib, n > 30, isPrime x]
