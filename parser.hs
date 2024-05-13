import System.IO
import Utils (splitBy)

predic :: String -> Bool
predic _ = True

-- SQL Types
data Query = Query Select (Where String) OrderBy
data Select = SelectAll | Column Int
data Where a = WhereA (a -> Bool) 
data OrderBy = NoOrder | Key Int

processQuery :: Query -> [[String]] -> [[String]]
processQuery (Query selection condition order) table =
       case selection of
              SelectAll-> table
              Column id -> map (\row -> [row !! id]) table

-- Processing
main = do
       handle <- openFile "test.csv" ReadMode
       contents <- hGetContents handle
       let (columns:table) = map (`splitBy` ',') $ lines contents
       let q = Query SelectAll (WhereA predic) NoOrder

       print $ processQuery q table