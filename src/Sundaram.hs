module Sundaram where

-- Sieve of Sundaram
-- サンダラムの篩
-- http://en.wikipedia.org/wiki/Sieve_of_Sundaram

-- package: llrbtree
import qualified Data.Set.LLRBTree as RB

primes :: (Num n, Ord n, Enum n, Integral n) => n -> [n]
primes limit = 2 : remainders'
  where remainders  = toOddNumbers $ filteredSource limit'
        remainders' = takeWhile (<=limit) remainders
        limit'      = limit `div` 2 + 1 

filteredSource :: (Eq n, Ord n, Num n, Enum n) => n -> [n]
filteredSource limit = filter isTarget numbers
  where excusion = fromN limit
        numbers  = [1..limit]
        exTree   = RB.fromList excusion
        isTarget = not . (`RB.member` exTree)

toOddNumbers :: (Num n) => [n] -> [n]
toOddNumbers = fmap toOdd

toOdd :: (Num n) => n -> n
toOdd = (+1) . (*2)

fromN :: (Num n, Ord n, Enum n) => n -> [n]
fromN n = do
  i <- takeWhile (lten . ii) [1..n]
  takeWhile lten $ map (x i) [i..n]
    where ii a  = 2 * a * ( 1 + a )
          x i j = i + j + 2 * i * j
          lten  = (<=n) 
