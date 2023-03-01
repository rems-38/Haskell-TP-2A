data EtatMachine = Vide | Percolation | CafePret Int deriving Show
type InfoMachine = (Int, EtatMachine)

lancerMachine :: InfoMachine -> InfoMachine
lancerMachine (n, state) = (n, Percolation)

attendre :: InfoMachine -> InfoMachine
attendre (n, state) = (n, CafePret 4)

servirCafe :: InfoMachine -> InfoMachine
servirCafe (n, CafePret nbr) = if (nbr > 1) then (n + 1, CafePret (nbr - 1)) else (n + 1, Vide)
servirCafe (n, state) = (n, state)

executerActions :: InfoMachine -> [Char] -> InfoMachine
executerActions infos [] = infos
executerActions infos (x:xs) = if x == 'L' then executerActions (lancerMachine infos) xs
                               else if x == 'A' then executerActions (attendre infos) xs
                               else if x == 'S' then executerActions (servirCafe infos) xs
                               else executerActions infos xs