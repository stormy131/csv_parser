import System.IO
import Data.List (sortBy)
import Utils (
       splitBy, 
       findIndices, 
       deleteAt, 
       formatTable)

type Query a = [Operator a]
data Operator a = Select [String] | 
                  Where String (a -> Bool) | 
                  OrderBy String  | 
                  Drop String

processQuery :: Query String -> [[String]] -> [[String]]
processQuery [] table = table
processQuery (first_op:rest) all_data@(columns:rows) = processQuery rest processed
       where processed = case first_op of
              Select names -> map (\row -> [row !! i | i <- column_ids]) all_data
                     where column_ids = findIndices names columns
              
              Where column p -> columns : filter (p . (!! i)) rows
                     where [i] = findIndices [column] columns

              OrderBy column -> columns : sortBy (\r1 r2 -> compare (r1 !! i) (r2 !! i)) rows
                     where [i] = findIndices [column] columns

              Drop column -> map (deleteAt i) all_data
                     where [i] = findIndices [column] columns
              

-- Processing
main = do
       handle <- openFile "test.csv" ReadMode
       contents <- hGetContents handle
       let table = map (`splitBy` ',') $ lines contents
       let q = [Select ["a", "c"], OrderBy "c"]

       putStrLn $ formatTable table
       putStr $ formatTable $ processQuery q table