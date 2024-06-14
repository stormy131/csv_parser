module Utils (
    splitBy,
    findIndices,
    deleteAt,
    formatTable
) where

import Data.List (elemIndex)

splitBy :: String -> Char -> [String]
splitBy [] _ = [""]
splitBy (x:xs) delim
    | x == delim = "" : rest
    | otherwise = (x : head rest) : tail rest
    where
        rest = splitBy xs delim

findIndices :: Eq a => [a] -> [a] -> [Int]
findIndices [] _ = []
findIndices (e:rest) list = idx : findIndices rest list
    where Just idx = elemIndex e list

deleteAt :: Int -> [a] -> [a]
deleteAt 0 (_:rest) = rest
deleteAt n (e:rest) = e : deleteAt (n - 1) rest

formatTable :: Show a => [[a]] -> String
formatTable = foldl (\ acc r -> acc ++ join r ++ "\n") ""
    where join = foldl (\acc e -> acc ++ show e ++ "\t") ""

