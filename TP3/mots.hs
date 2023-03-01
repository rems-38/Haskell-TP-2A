espace :: Char -> Bool
espace x = if x == ' ' then True else False

separe :: (Char -> Bool) -> String -> (String, String)
separe op (x:xs) = if op x == True then (x : separe op xs, xs) else ([x], xs)