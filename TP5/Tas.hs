data Tas a = Noeud Int a (Tas a) (Tas a) | Vide deriving (Show)

t1 :: Tas Int
t1 = Noeud 8 (-8) (Noeud 3 1 (Noeud 1 5 Vide Vide) (Noeud 1 42 Vide Vide)) (Noeud 4 (-3) (Noeud 2 28 Vide (Noeud 1 60 Vide Vide)) (Noeud 1 7 Vide Vide))

taille :: Tas a -> Int
taille Vide = 0
taille (Noeud n _ _ _) = n

tas_min :: Tas a -> a
tas_min (Noeud _ n _ _) = n

noeud :: a -> Tas a -> Tas a -> Tas a
noeud val gauche droite = (Noeud (1 + (taille gauche) + (taille droite)) val gauche droite)

est_equilibre :: Tas a -> Bool
est_equilibre Vide = True
est_equilibre (Noeud _ val gauche droite) = (abs ((taille gauche) - (taille droite)) <= 1) && est_equilibre gauche && est_equilibre droite

ajouter :: a -> Tas a -> Tas a

ajouter val (Noeud _ v gauche droite) = if (taille gauche) <= (taille droite)
                                            then if v >= val
                                                then noeud v g2 droite
                                                else noeud val g2 droite
                                            else if v >= val
                                                then noeud v gauche d2
                                                else noeud val gauche d2
                                        where g2 = ajouter (tas_min gauche) (noeud val ......)
                                              d2 = ajouter (tas_min droite) (........)