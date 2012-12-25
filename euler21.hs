import Data.List (group, sort)
import Data.Numbers.Primes (primeFactors)

main = do
	 let res = sum $ filter isAmicableNumber  [1..9999] -- 1.359 sec
	 print res

isAmicableNumber a = (a /= b) && (a == a')
    where b = sum $ properDivisors a
          a' = sum $ properDivisors b

properDivisors 0 = [0]
properDivisors n = init $ factors n

--
-- Function calculates how many factors the given number has.
-- http://mathschallenge.net/index.php?section=faq&ref=number/number_of_divisors
--
-- Listing Factors of large numbers?
-- 1. Obtain the unique prime factorization.
--    primeFactors 18000 == [2,2,2,2,3,3,5,5,5]
-- 2. Express this as powers
--    (2^4)*(3^2)*(5^3)
-- 3. Determine how many factors there are, just look at each power, add one, and multiply together.
--    (4+1)*(2+1)*(3+1) == 60 factors
-- 4. To list what factors actually are, just use the digits 0 to p, where p is the power,
--    to list all possible combinations.
--    Notice that, as powers, we have a 4, a 2, and a 3.
--    So we'd start off as
--    0 0 0
--    0 0 1
--    0 0 2
--    0 0 3
--    Then you'd bump up the second column
--    0 1 0
--    0 1 1
--    0 1 2
--    0 1 3
--    What these numbers represents are:
--    (2^0)*(3^0)*(5^0) as the first factor
--    (2^0)*(3^0)*(5^1) as the second
--    (2^0)*(3^0)*(5^2) third
--    (2^0)*(3^0)*(5^3) fourth
--    and you keep going until you reach 4 2 3.
--
factorsCount :: (Integral a) => a -> Int
factorsCount n = foldl (\acc (_, p) -> acc * (p + 1)) 1
               $ map (\xs@(x:_) -> (x, length xs))
               $ group
               $ primeFactors n

--
-- Function lists all factors on the given number (quite fast).
--
factors :: (Integral a) => a -> [a]
factors n
    | n == 0    = []
	| n == 1    = [1]
	| n < 0     = factors $ abs n
	| otherwise = sort $ factors' n
    where factors' x = map product
                     $ cartesianProduct
                     $ map (\xs@(x:_) -> [x^p | p <- [0..length xs]])
                     $ group
                     $ primeFactors x

cartesianProduct :: [[a]] -> [[a]]
cartesianProduct [] = [[]]
cartesianProduct (set:sets) = let cp = cartesianProduct sets
                              in [x : xs | x <- set, xs <- cp]

-- The triangular number Tn=n+(n-1)+...+2+1 is therefore the additive
-- analog of the factorial n!=n*(n-1)...2*1.

triangularNumbers = map triangularNumber [1..]
triangularNumber n = round $ n * (n + 1) / 2

triangularNumberIndex :: Integer -> Integer
triangularNumberIndex n = (1+) . fromIntegral . length . fst $ break (==n) triangularNumbers

triangularNumbersTable :: [(Integer, Integer)]
triangularNumbersTable = zip [1..] triangularNumbers
