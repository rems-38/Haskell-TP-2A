data Couleur = Pique | Coeur | Carreau | Trèfle deriving Show
data Carte = Standard Couleur Int | Atout Int | Excuse deriving Show

instance Eq Couleur where
    (==) Pique Pique = True
    (==) Coeur Coeur = True
    (==) Carreau Carreau = True
    (==) Trèfle Trèfle = True
    (==) _ _ = False

comparer :: Carte -> Carte -> Bool
comparer (Standard c1 x) (Standard c2 y) = if c1 == c2 then x >= y else False
comparer (Atout x) (Atout y) = x >= y
comparer (Atout _) (Standard _ _) = True
comparer (Standard _ _) (Atout _) = False
comparer Excuse _ = False
comparer _ Excuse = True