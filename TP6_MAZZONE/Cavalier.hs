import ProblemeCavalier

mouvements :: Etat -> [Position]
mouvements (board, (x, y)) = filter good_couple [(x+dx, y+dy) | (dx, dy) <- [(1,2), (1,-2), (-1,2), (-1,-2), (2,1), (2,-1), (-2,1), (-2,-1)]]
                            where good_couple (x, y) = position_valide (x, y) && (not (position_visitee board (x, y)))

etats_suivants :: Etat -> [Etat]
etats_suivants state = do
  mouv <- mouvements state
  let new_plateau = visiter (fst state) mouv
  return (new_plateau, mouv)

explorer_etats :: Etat -> [Etat]
explorer_etats state | plateau_plein (fst state) = [state]
                     | otherwise = do
                       next_etat <- etats_suivants state
                       explorer_etats next_etat

etats_gagnants :: Position -> [Etat]
etats_gagnants p = explorer_etats (plateau_vide, p)

positions_initiales_realisables :: [Position]
positions_initiales_realisables = unique [(x,y) | x <- [0..largeur-1], y <- [0..hauteur-1], _ <- etats_gagnants (x,y)]

unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs) = x : unique (filter (/= x) xs)