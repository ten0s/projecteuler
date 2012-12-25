import Data.List (permutations)
import Data.Numbers.Primes (isPrime)

main = do
	 let res = maximum $ filter isPrime $ map stringToInt $ permutations "1234567"
	 print res

stringToInt :: String -> Int
stringToInt s = read s

