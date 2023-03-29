data Tas a = Noeud Int a (Tas a) (Tas a) | Vide deriving (Show)

t1 :: Tas Int
t1 = Noeud 8 (-8) (Noeud 3 1 (Noeud 1 5 Vide Vide) (Noeud 1 42 Vide Vide)) (Noeud 4 (-3) (Noeud 2 28 Vide (Noeud 1 60 Vide Vide)) (Noeud 1 7 Vide Vide))

taille :: Tas a -> Int
taille Vide = 0
taille (Noeud n _ _ _) = n

tas_min :: Tas a -> a
tas_min (Noeud _ v _ _) = v

noeud :: a -> Tas a -> Tas a -> Tas a
noeud val gauche droite = (Noeud (1 + (taille gauche) + (taille droite)) val gauche droite)

est_equilibre :: Tas a -> Bool
est_equilibre Vide = True
est_equilibre (Noeud _ val gauche droite) = (abs ((taille gauche) - (taille droite)) <= 1) && est_equilibre gauche && est_equilibre droite

ajouter :: Ord a => a -> Tas a -> Tas a
ajouter val Vide = Noeud 1 val Vide Vide
ajouter val (Noeud _ v gauche droite) = if (taille gauche) <= (taille droite)
                                            then if v <= val
                                                then noeud v (ajouter val gauche) droite
                                                else noeud val (ajouter v gauche) droite
                                            else if val <= v
                                                then noeud v gauche (ajouter val droite)
                                                else noeud val gauche (ajouter v gauche)

retirer_feuille :: Tas a -> (a, Tas a)
retirer_feuille Vide = error "Erreur : Le tas est vide"
retirer_feuille (Noeud n v Vide Vide) = (v, Vide)
retirer_feuille (Noeud n v gauche droite) | taille gauche <= taille droite = (fst $ retirer_feuille droite, (Noeud (n-1) v gauche (snd $ retirer_feuille droite)))
                                          | otherwise = (fst $ retirer_feuille gauche, (Noeud (n-1) v (snd $ retirer_feuille gauche) droite))

equilibrer :: Ord a => Tas a -> Tas a
equilibrer Vide = Vide
equilibrer tas@(Noeud _ val gauche droite) | est_equilibre tas = tas
                                           | taille gauche > taille droite = equilibrer $ noeud (tas_min tas) (snd (retirer_feuille gauche)) (ajouter (fst (retirer_feuille gauche)) droite)
                                           | otherwise = equilibrer $ noeud (tas_min tas) (ajouter (fst (retirer_feuille droite)) gauche) (snd (retirer_feuille droite))

retirer :: Ord a => Tas a -> Tas a
retirer Vide = Vide
retirer (Noeud _ _ gauche Vide) = gauche
retirer (Noeud _ _ Vide droite) = droite
retirer (Noeud n _ gauche droite) | tas_min gauche <= tas_min droite = equilibrer $ Noeud (n-1) (tas_min gauche) (retirer gauche) droite
                                  | otherwise = equilibrer $ Noeud (n-1) (tas_min droite) gauche (retirer droite)

construit :: Ord a => [a] -> Tas a
construit [] = Vide
construit (x:xs) = equilibrer $ noeud x (construit xs) Vide

deconstruit :: Ord a => Tas a -> [a]
deconstruit Vide = []
deconstruit (Noeud _ val gauche droite) = val : deconstruit gauche ++ deconstruit droite

tri :: Ord a => [a] -> [a]
tri tas = (deconstruit . construit)