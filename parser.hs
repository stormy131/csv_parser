import System.IO
import Data.List (sortBy)
import Utils (
       splitBy, 
       findIndices, 
       deleteAt, 
       formatTable)

data Operator a = Select [a] |
                  Where a (a -> Bool) | 
                  OrderBy a  | 
                  Drop a

class DataFilter f where
       process :: (Eq a, Ord a) => f a -> [[a]] -> [[a]]

instance DataFilter Operator where
       process (Select names) all_data@(columns:_) = map (\row -> [row !! i | i <- column_ids]) all_data
              where column_ids = findIndices names columns

       process (Where column p) (columns:rows) = columns : filter (p . (!! i)) rows
              where [i] = findIndices [column] columns

       process (OrderBy column) (columns:rows) = columns : sortBy (\r1 r2 -> compare (r1 !! i) (r2 !! i)) rows
              where [i] = findIndices [column] columns

       process (Drop column) all_data@(columns:_) = map (deleteAt i) all_data
              where [i] = findIndices [column] columns

processQuery :: (DataFilter b, Ord a) => [[a]] -> [b a] -> [[a]]
processQuery = foldl (flip process)

-- Processing
main :: IO ()
main = do
       print "Input file:"
       input_file <- getLine 

       print "Output file:"
       output_file <- getLine

       print "Data query:"
       query_str <- getLine

       handle <- openFile input_file ReadMode
       contents <- hGetContents handle
       let table = map (`splitBy` ',') $ lines contents
       let q = [Select ["a", "c"], OrderBy "a"]

       if null output_file
              then putStrLn $ formatTable $ processQuery table q
              else writeFile output_file $ formatTable $ processQuery table q