repeter_n_fois1 :: Int -> Char -> String
repeter_n_fois1 n c = [c | _ <- [1..n]]

repeter_n_fois :: Int -> a -> [a]
repeter_n_fois n x = [x | _ <- [1..n]]

etoiles :: Int -> String -> String
etoiles n s = repeter_n_fois n '*' ++ s ++ repeter_n_fois n '*'

slashes :: String -> String
slashes s = '/' : s ++ "/"

espace :: String -> String
espace s = ' ' : s ++ [' ']

commentaire_documentation :: String -> String
commentaire_documentation = slashes . (etoiles 2) . espace