import Data.List (nub)
import Data.Numbers.Primes (primeFactors)

main = do
	 let res = lcm' [1..20] -- instantly :)
	 print res

--
-- Least Common Multiple
-- http://mathforum.org/library/drmath/view/62527.html
--
{-
2 :  2^1
3 :        3^1
4 :  2^2
5 :              5^1
6 :  2^1   3^1
7 :                    7^1
8 :  2^3
9 :        3^2
10:  2^1         5^1
--------------------------
     2^3 * 3^2 * 5^1 * 7^1 == 8 * 9 * 5 * 7 == 2520
-}
lcm' list = foldl (\res (b, e) -> res * b^e) 1 pairs
    where pairs = map (\x -> (x, maxFreq x)) primeBases
          maxFreq x = foldl (\acc xs -> let freq = length $ takeWhile (==x) xs
                                        in if acc < freq then freq else acc) 0 primeFactors'
          primeBases = nub $ concat primeFactors'
          primeFactors' = map primeFactors list
