module Sundaram where
import Control.Monad
import Data.List
 
fromN :: Int -> [Int]
fromN n = do
  i <- takeWhile (\a -> ii a <= n) [1..n]
  takeWhile      (\j -> x i j <=n) [i..n]
    where ii a = 2 * a * (a + 1)
          x i j = i + j + 2 * i * j 