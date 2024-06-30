import CSVLib
import Control.Exception

main :: IO ()
main = do
    -- Reading CSV data from file
    data_a <- readCSV "test_data/a.csv" ','
    
    putStrLn "XXX Data A: XXX"
    putStrLn $ formatTable '\t' data_a


    let q1 = [Select ["a", "b"]]
    putStrLn "--> Select: columns a,b"
    putStrLn $ formatTable '\t' $ processQuery data_a q1

    let q2 = [Where "a" (\x -> (read x :: Integer) > 2)]
    putStrLn "--> Where: 'a' values bigger than 2"
    putStrLn $ formatTable '\t' $ processQuery data_a q2

    -- Throws an ERROR
    -- let q3 = [Where "X" (== "X")]
    -- putStrLn "--> Where: condition on unavailable column"
    -- catch (print $ processQuery data_a q3) putStrLn
     

    let q4 = [OrderBy "c"]
    putStrLn "--> OrderBy: ordering by values in 'c'"
    putStrLn $ formatTable '\t' $ processQuery data_a q4

    -- Throws an ERROR
    -- let q5 = [OrderBy "X"]
    -- putStrLn "--> OrderBy: ordering by values in unavilable column"
    -- putStrLn $ formatTable '\t' $ processQuery data_a q5

    let q6 = [Select ["a", "b", "c"], Drop "b"]
    putStrLn "--> Drop: removes column"
    putStrLn $ formatTable '\t' $ processQuery data_a q6

    -- Throws an ERROR
    -- let q7 = [Select ["a", "b", "c"], Drop "X"]
    -- putStrLn "--> Drop: removes unavailable column"
    -- putStrLn $ formatTable '\t' $ processQuery data_a q7

    data_b <- readCSV "test_data/b.csv" ','
    putStrLn "XXX Data B XXX"
    putStrLn $ formatTable '\t' data_b

    let q8 = [ConcatRows data_b]
    putStrLn "--> ConcatRows: appends B rows to the A"
    putStrLn $ formatTable '\t' $ processQuery data_a q8

    -- Throws an ERROR
    -- let q9 = [ConcatRows $ processQuery data_b [Select ["a", "b"]]]
    -- putStrLn "--> ConcatRows: same appending + inconsistent table columns"
    -- putStrLn $ formatTable '\t' $ processQuery data_a q9

    data_c <- readCSV "test_data/c.csv" ','
    putStrLn "XXX Data C XXX"
    putStrLn $ formatTable '\t' data_c

    let q10 = [ConcatCols data_c]
    putStrLn "--> ConcatCols: append columns in C to A"
    putStrLn $ formatTable '\t' $ processQuery data_a q10

    let q11 = [ConcatCols $ processQuery data_c [Where "d" (\x -> (read x :: Integer) < 1)]]
    putStrLn "--> ConcatCols: same appending + data shrinkage"
    putStrLn $ formatTable '\t' $ processQuery data_a q11

    putStrLn "--> Testing file output"
    writeCSV "test_data/output.csv" ',' $ processQuery data_a q11
    putStrLn "Executed the last test again and wrote the result to the 'test_data/output.csv'"