espace :: Char -> Bool
espace x = x == ' '

left :: (a, b) -> a 
left (x, _) = x
right :: (a, b) -> b
right (_, x) = x

separe :: (Char -> Bool) -> String -> (String, String)
separe op s = (left separation, dropWhile op (right separation))
    where separation = span (not . op) s

grignote_espaces :: String -> String
grignote_espaces [] = ""
grignote_espaces (x:xs)
    | espace x = grignote_espaces xs
    | otherwise = x:xs

un_mot :: String -> (String, String)
un_mot mot = (left function, grignote_espaces (right function))
    where function = separe espace (grignote_espaces mot)

mots :: String -> [String]
mots [] = []
mots mot = (left (un_mot mot)) : mots (right (un_mot mot))