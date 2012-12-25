import Data.Numbers.Primes (isPrime, primes)

main = do
	 let res = length $ filter isRotatedPrime $ takeWhile (<1000000) primes
	 print res

isRotatedPrime p = all isPrime ps
    where s = show p
          ps = map stringToInt $ map (rotate s) [0..length s - 1]

rotate :: [a] -> Int -> [a]
rotate xs n
    | null xs = xs
    | n > 0 = rotateLeft xs n
    | n < 0 = rotateRight xs n
    | otherwise = xs
    where rotateLeft ys 0 = ys
          rotateLeft (y:ys) n = rotateLeft (ys ++ [y]) (n - 1)
          rotateRight ys 0 = ys
          rotateRight ys n = rotateRight (last ys : init ys) (n + 1)

stringToInt :: String -> Int
stringToInt s = read s
