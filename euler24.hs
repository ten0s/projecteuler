import Data.List (permutations, sort)

main = do
	 let res = (sort $ permutations "0123456789") !! 999999
	 print res
