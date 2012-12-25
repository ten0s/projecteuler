import Data.List (intersect, nub, permutations)
import Data.Numbers.Primes (isPrime, primes)

-- >[[1487,4817,8147],[2969,6299,9629]] => 296962999629
main = do
	 let res = filter (not.null) $ map findArithmSeq seqs
	 print res

fourDigitsPrimes = takeWhile ((5>).digits) $ dropWhile ((4>).digits) $  primes

primePerms :: [Integer] -> Integer -> [Integer]
primePerms ps p = let perms = map stringToInteger $ permutations $ show p
                  in ps `intersect` perms

seqs = filter ((2<).length) $ nub $ map (primePerms fourDigitsPrimes) fourDigitsPrimes

findArithmSeq :: (Num a) => [a] -> [a]
findArithmSeq [] = []
findArithmSeq (_:[]) = []
findArithmSeq (a:b:xs) = if c `elem` xs
                         then [a, b, c]
                         else if not $ null removeFirst
                              then removeFirst
                              else removeSecond
    where c = (b - a) + b
          removeFirst = findArithmSeq (b:xs)
          removeSecond = findArithmSeq (a:xs)

findArithmSeq' s = [[a,b,c] | a <- s, b <- s, c <- s, a < b, b < c, a < c, a - b == b - c]

digits :: (Integral a) => a -> Int
digits = length . show

stringToInteger :: String -> Integer
stringToInteger s = read s
