import Control.Monad
import Control.Monad.Writer

main = do
    contents <- readFile "files/p018_triangle.txt"
    let triangle = map (address 0)
                 $ map ((map stringToInt).words)
                 $ (reverse.lines) contents
    let result = censor reverse $ head $ solve triangle
    let (res, _) = runWriter result
    print $ res

stringToInt :: String -> Int
stringToInt s = read s

solve :: (Num a, Ord a, Monoid w) => [[Writer w a]] -> [Writer w a]
solve [] = error "solve: No variants provided"
solve [x] = x
solve (t:n:xs) = let t' = shrink t
                     n' = zipWith (liftM2 (+)) t' n
                 in solve (n':xs)

address :: (Enum w) => w -> [a] -> [Writer [w] a]
address _ []     = []
address a (x:xs) = writer (x, [a]) : address (succ a) xs

shrink :: (Ord a, Monoid w) => [Writer w a] -> [Writer w a]
shrink [] = []
shrink [_] = []
shrink (w1:w2:ws) = choose w1 w2 : shrink (w2:ws)

choose :: (Ord a, Monoid w) => Writer w a -> Writer w a -> Writer w a
choose m1 m2 = if v1 > v2 then m1 else m2
  where v1 = fst $ runWriter m1
        v2 = fst $ runWriter m2
