import ListAux

main = do
	 let res = middle $ pascalRow (20 * 2)
	 print res

-- http://mathschallenge.net/index.php?section=problems&show=true&titleid=random_routes&full=true
-- http://mathforum.org/advanced/robertd/manhattan.html

pascalRow :: Int -> [Integer]
pascalRow 0 =  [1]
pascalRow n = 1 : row (pascalRow (n - 1))
    where row (x:[]) = [x]
          row (x:y:xs) = (x + y) : row (y:xs)
