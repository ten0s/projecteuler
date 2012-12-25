import Data.List (elemIndex, sort)
import Data.Maybe (fromJust)
import ListAux

main = do
    contents <- readFile "problem22.txt"
    let names = sort $ map (tail.init) $ wordsWith (== ',') contents
    let charsSum = foldl (\acc c -> acc + (fromJust (elemIndex c ['A'..'Z']) + 1 {-use index starting 1-})) 0
    let nameIndex n = fromJust (elemIndex n names) + 1{-use index starting 1-}
    let res = foldl (\acc name -> let chars = charsSum name
                                      index = nameIndex name
                                      in acc + chars * index) 0 names
    print res
