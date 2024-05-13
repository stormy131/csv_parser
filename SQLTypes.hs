module SQLTypes (
    Select,
    Where,
    OrderBy,
    Query
) where

-- data Query = Query {
--     select :: Select,
--     condition :: Where String,
--     sort :: OrderBy
-- }

data Query = QueryA Select (Where String) OrderBy
data Select = SelectAll | Column Int
data Where a = WhereA (a -> Bool) 
data OrderBy = NoOrder | Key Int

processQuery q table =
       case select q of
            SelectAll -> table
            Column idx -> map (\row -> [row !! idx]) table