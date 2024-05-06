module Utils (
    splitBy
) where

splitBy :: String -> Char -> [String]
splitBy [] _ = [""]
splitBy (x:xs) delim
    | x == delim = "" : rest
    | otherwise = (x : head rest) : tail rest
    where
        rest = splitBy xs delim