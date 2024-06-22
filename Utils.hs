module Utils (
    splitBy,
    findIndices,
    deleteAt,
    formatTable
) where

import Data.List (elemIndex)
import Data.Maybe (isNothing)

splitBy :: String -> Char -> [String]
splitBy [] _ = [""]
splitBy (x:xs) delim
    | x == delim = "" : rest
    | otherwise = (x : head rest) : tail rest
    where
        rest = splitBy xs delim

findIndices :: (Eq a, Show a) => [a] -> [a] -> [Int]
findIndices [] _ = []
findIndices (e:rest) list
    | isNothing $ elemIndex e list = error $ "Column " ++ show e ++ " is not available "
    | otherwise = idx : findIndices rest list
    where Just idx = elemIndex e list

deleteAt :: Int -> [a] -> [a]
deleteAt 0 (_:rest) = rest
deleteAt n (e:rest) = e : deleteAt (n - 1) rest

-- Internally, data wil always be in String format
formatTable :: Char -> [[String]] -> String
formatTable separator = foldl (\acc r -> acc ++ join r ++ "\n") ""
    where join = foldl1 (\acc e -> acc ++ (separator : e))