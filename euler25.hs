import Data.List (permutations, sort)

main = do
	 let res = fst.head $ dropWhile ((<1000).length.show.snd) $ zip [1..] (1:fibs)
	 print res

fib n = fibs !! n
fibs = 1 : 2 : zipWith (+) fibs (tail fibs)
