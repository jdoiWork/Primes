module Eratosthenes where

-- Sieve of Eratosthenes
-- エラトステネスの篩
-- http://en.wikipedia.org/wiki/Sieve_of_Eratosthenes

primes :: (Integral n) => n -> [n]
primes limit = sieve limit [2..limit] []

sieve :: (Integral n) => n -> [n] -> [n] -> [n]
sieve _ []     ps             = reverse ps
sieve z ns     ps@(p:_) | zpp = reverse ps ++ ns
  where zpp = z < p * p
sieve z (n:ns) ps             = sieve z ns' ps'
  where ps' = n:ps
        ns' = filter f ns
        f x = x `mod` n /= 0
