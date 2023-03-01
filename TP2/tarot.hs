data Couleur = Pique | Coeur | Carreau | Trèfle deriving Show
data Carte = Standard Couleur Int | Atout Int | Excuse deriving Show

instance Eq Couleur where
    (==) Pique Pique = True
    (==) Coeur Coeur = True
    (==) Carreau Carreau = True
    (==) Trèfle Trèfle = True
    (==) _ _ = False

instance Eq Carte where
    (==) (Standard c1 x1) (Standard c2 x2) = if c1 == c2 && x1 == x2 then True else False
    (==) (Atout x1) (Atout x2) = if x1 == x2 then True else False
    (==) Excuse Excuse = True
    (==) (Standard _ _) _ = False
    (==) (Atout _) _ = False
    (==) Excuse _ = False

comparer :: Carte -> Carte -> Bool
comparer (Standard c1 x) (Standard c2 y) = if c1 == c2 then x >= y else False
comparer (Atout x) (Atout y) = x >= y
comparer (Atout _) (Standard _ _) = True
comparer (Standard _ _) (Atout _) = False
comparer Excuse _ = False
comparer _ Excuse = True

meme_couleur :: Couleur -> Carte -> Bool
meme_couleur c (Standard c2 _) = c == c2
meme_couleur _ _ = False

atout_plus_grand :: Int -> Carte -> Bool
atout_plus_grand x (Atout y) = y >= x
atout_plus_grand _ _ = False

atout :: Carte -> Bool
atout (Atout _) = True
atout _ = False

(|||) :: (Eq a) => [a] -> [a] -> [a]
(|||) l1 l2 = if l1 /= [] then l1 else l2

cartes_possibles_hors_excuse :: Carte -> [Carte] -> [Carte]
cartes_possibles_hors_excuse (Standard c _) cartes = (filter (meme_couleur c) cartes ||| filter atout cartes) ||| cartes
cartes_possibles_hors_excuse (Atout x) cartes = (filter (atout_plus_grand x) cartes ||| filter atout cartes) ||| cartes


is_excuse :: Carte -> Bool
is_excuse Excuse = True
is_excuse _ = False

cartes_possibles :: Carte -> [Carte] -> [Carte]
cartes_possibles carte@(Standard _ _) cartes = (filter is_excuse cartes) ++ (cartes_possibles_hors_excuse carte cartes)
cartes_possibles carte@(Atout _) cartes = (filter is_excuse cartes) ++ (cartes_possibles_hors_excuse carte cartes)
cartes_possibles Excuse cartes = cartes
