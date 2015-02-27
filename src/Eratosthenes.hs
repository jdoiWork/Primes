module Eratosthenes where

-- Sieve of Eratosthenes
-- エラトステネスの篩
-- http://en.wikipedia.org/wiki/Sieve_of_Eratosthenes

primes :: (Integral n) => n -> [n]
primes limit = reverse $ sieve [2..limit] []

sieve :: (Integral n) => [n] -> [n] -> [n]
sieve []     ps = ps
sieve (n:ns) ps = sieve (filter f ns) (n:ps)
  where f x = x `mod` n /= 0
