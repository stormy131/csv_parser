module CSVLib (
       Operator(..),
       DataModifier,
       readCSV,
       writeCSV,
       formatTable,
       processQuery
) where

import System.IO
import Utils
import Data.List (sortBy)
import System.Directory (doesFileExist)

data Operator a = Select [a] |
                  Where a (a -> Bool) | 
                  OrderBy a  | 
                  Drop a |
                  ConcatRows [[a]] |                 -- Concat "tables" along 0 axis (adds rows)
                  ConcatCols [[a]]                   -- Concat "tables" along 1 axis (adds columns)

class DataModifier operator where
       process :: (Eq a, Ord a, Show a) => operator a -> [[a]] -> [[a]]

instance DataModifier Operator where
       process _ [] = []
       
       process (Select names) all_data@(columns:_) = map (\row -> [row !! i | i <- column_ids]) all_data
              where column_ids = findIndices names columns

       process (Where column p) (columns:rows) = columns : filter (p . (!! i)) rows
              where [i] = findIndices [column] columns

       process (OrderBy column) (columns:rows) = columns : sortBy (\r1 r2 -> compare (r1 !! i) (r2 !! i)) rows
              where [i] = findIndices [column] columns

       process (Drop column) all_data@(columns:_) = map (deleteAt i) all_data
              where [i] = findIndices [column] columns

       process (ConcatRows (cols_a:rest_a)) (cols_b:rest_b)
              | cols_a == cols_b = cols_b : rest_b ++ rest_a
              | otherwise = error "Columns do not match [0-axis concatenation]"

       process (ConcatCols data_a) data_b = map (uncurry (++)) overlap
              where overlap = zip data_b data_a

processQuery :: (DataModifier b, Ord a, Show a) => [[a]] -> [b a] -> [[a]]
processQuery = foldl (flip process)

readCSV :: String -> Char -> IO [[String]]
readCSV file_path separator = do
       valid <- doesFileExist file_path
       if not valid
              then error $ "Provided filepath \"" ++ file_path ++ "\" is invalid."
              else do
                     content <- readFile file_path
                     let file_lines = lines content
                     let csv_data = map (`splitBy` separator) file_lines
                     return csv_data

writeCSV :: String -> Char -> [[String]] -> IO ()
writeCSV file sep values = do
       writeFile file $ formatTable sep values