import Data.Numbers.Primes (primeFactors)

main = do
	 let res = maximum $ primeFactors 600851475143
	 print res
