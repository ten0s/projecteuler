import Data.Numbers.Primes (primes)

main = do
	 let res = primes !! (10001-1)
	 print res
