import Data.Numbers.Primes (primes)

main = do
	 let res = sum $ takeWhile (<2000000) primes
	 print res
