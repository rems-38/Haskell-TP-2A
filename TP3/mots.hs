espace :: Char -> Bool
espace x = if x == ' ' then True else False

left :: (a, b) -> a 
left (x, _) = x
right :: (a, b) -> b
right (_, x) = x

separe :: (Char -> Bool) -> String -> (String, String)
separe op [] = ([], [])
separe op l@(x:xs)
    | op x = (x:(left reste), right reste) 
    | otherwise = ([], l)
    where
        reste = separe op xs