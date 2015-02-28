module Atkin (primes) where

-- Sieve of Atkin
-- アトキンの篩
-- http://en.wikipedia.org/wiki/Sieve_of_Atkin
-- http://stackoverflow.com/questions/10580159/sieve-of-atkin-explanation-and-java-example

import Data.Array.Diff (DiffUArray, array, assocs, accum, (//))
-- import Data.Array (Array, array, assocs, accum, (//))
import Data.List  (foldl')

type IntX  = Integer
type Sieve = DiffUArray IntX Bool
-- type Sieve = Array IntX Bool
type RF    = IntX -> IntX -> IntX -> [IntX]

primes :: IntX -> [IntX]
primes limit = map fst . filter snd $ assocs sieve''
  where flipers = flips limit
        sieve   = initSieve $ makeBitmap limit
        sieve'  = flipSieve sieve flipers
        sieve'' = removeSieve limit sieve'

removeSieve :: IntX -> Sieve -> Sieve
removeSieve l1 sieve =
  foldl' f sieve [5..l2] where
    l2 = iSqrt l1
    f s n = s // is where
      x   = n * n
      xs  = takeWhile (<=l1) $ map (x*) [1..]
      is  = falses xs

initSieve :: Sieve -> Sieve
initSieve sieve = sieve // trues [2, 3]

flipSieve :: Sieve -> [IntX] -> Sieve
flipSieve sieve flipers =
  accum (\e _ -> not e) sieve (trues flipers)

makeBitmap :: IntX -> Sieve
makeBitmap limit = array (2, limit) (falses [2..limit])

flips :: IntX -> [IntX]
flips limit = do
  x <- [1..limitSqrt]
  y <- [1..limitSqrt]
  r <- [r1, r2, r3  ]
  f <- r limit x y
  return f
  where
    limitSqrt = iSqrt limit

iSqrt :: IntX -> IntX
iSqrt = floor . sqrt . fromIntegral

r1 :: RF
r1 limit x y = 
  (n <= limit && ((n % 12) == 1 || (n % 12) == 5)) <?> n
    where n = ((4 * x * x) + (y * y))

r2 :: RF
r2 limit x y =
  (n <= limit && (n % 12 == 7)) <?> n
    where n = (3 * x * x) + (y * y)

r3 :: RF
r3 limit x y =
  (x > y && n <= limit && (n % 12 == 11)) <?> n
    where n = (3 * x * x) - (y * y)

(<?>) :: Bool -> IntX -> [IntX]
True  <?> n = [n]
False <?> _ = [ ]

(%) :: IntX -> IntX -> IntX
(%) = mod

falses :: [IntX] -> [(IntX, Bool)]
falses = tff False

trues :: [IntX] -> [(IntX, Bool)]
trues = tff True

tff :: Bool -> [IntX] -> [(IntX, Bool)]
tff tf = \is -> [(i, tf) | i <- is]
