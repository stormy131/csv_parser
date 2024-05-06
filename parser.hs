import System.IO
import Utils (splitBy)

main = do
       handle <- openFile "test.csv" ReadMode
       contents <- hGetContents handle
       let result = map (`splitBy` ',') $ lines contents
       print result 