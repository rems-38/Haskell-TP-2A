module ProblemeCavalier where

-- Taille du plateau
largeur = 4
hauteur = 3

-- Une valeur de type Plateau encode l'ensemble des cases visitées par le
-- cavalier. Le fait que ce soit un entier n'importe pas, toutes les fonctions
-- pour créer et manipuler les valeurs de type Plateau sont fournies par ce
-- module.
type Plateau = Integer
-- Position sur le plateau sous la forme (x,y):
--   x est la colonne, numérotée de gauche à droite (entre 0 et largeur-1)
--   y est la ligne, numérotée de bas en haut (entre 0 et hauteur-1)
type Position = (Int, Int)
-- État du jeu : cases visitées sur le plateau + position actuelle du cavalier
type Etat = (Plateau, Position)

-- Plateau sur lequel le cavalier n'a pas encore été posé (utilisé pour l'état
-- initial du problème)
plateau_vide :: Plateau
plateau_vide = 0

-- Détermine si toutes les cases ont été visitées (ie. la partie est gagnée)
plateau_plein :: Plateau -> Bool
plateau_plein p = (p+1 == 2^(largeur*hauteur))

-- Indique si une position est bien dans les bornes de l'échiquier
position_valide :: Position -> Bool
position_valide (x, y) = x >= 0 && x < largeur && y >= 0 && y < hauteur

-- Marque une position comme visitée sur le plateau
visiter :: Plateau -> Position -> Plateau
visiter p (x, y) = p + 2 ^ (largeur * y + x)

-- Indique si une position a été visitée
position_visitee :: Plateau -> Position -> Bool
position_visitee p (x, y) = div p (2 ^ (largeur * y + x)) `mod` 2 /= 0
