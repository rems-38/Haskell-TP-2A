data Bintree a = Leaf a | Node a (Bintree a) (Bintree a) deriving Show

arbre_exemple :: Bintree Int
arbre_exemple = Node (-8) (Node 18 (Leaf (-5)) (Leaf 42)) (Node 13 (Node 28 (Leaf 4) (Leaf 0)) (Leaf 73))

nombre_feuilles :: Bintree a -> Int
nombre_feuilles (Leaf _) = 1
nombre_feuilles (Node _ gauche droite) = (nombre_feuilles gauche) + (nombre_feuilles droite)

hauteur :: Bintree a -> Int
hauteur (Leaf _) = 1
hauteur (Node _ gauche droite) = 1 + max (hauteur gauche) (hauteur droite)

abr_exemple :: Bintree Integer
abr_exemple = Node 18 (Node (-5) (Leaf (-8)) (Node 4 (Leaf 0) (Leaf 13))) (Node 42 (Leaf 28) (Leaf 73))

rechercher_abr :: Bintree Integer -> Integer -> Bool
rechercher_abr (Leaf v) x = if v == x then True else False
rechercher_abr (Node tete gauche droite) x = if x > tete then (rechercher_abr droite x) else (rechercher_abr gauche x)

aplatir_abr :: Bintree a -> [a]
aplatir_abr (Leaf v) = [v]
aplatir_abr (Node tete gauche droite) = (aplatir_abr gauche) ++ [tete] ++ (aplatir_abr droite)

