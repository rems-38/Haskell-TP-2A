data Bintree a = Leaf a | Node a (Bintree a) (Bintree a) deriving Show

arbre_exemple :: Bintree Int
arbre_exemple = Node (-8) (Node 18 (Leaf (-5)) (Leaf 42)) (Node 13 (Node 28 (Leaf 4) (Leaf 0)) (Leaf 73))

parcours_infixe :: Bintree a -> [a]
parcours_infixe (Leaf end) = [end]
parcours_infixe (Node noeud gauche droite) = parcours_infixe gauche ++ [noeud] ++ parcours_infixe droite

foldr_arbre :: (a -> b -> b) -> b -> Bintree a -> b
foldr_arbre op neutre (Leaf end) = op end neutre
foldr_arbre op neutre (Node noeud gauche droite) = foldr_arbre op (op noeud (foldr_arbre op neutre droite)) gauche

instance Foldable Bintree where
    foldr = foldr_arbre 

to_list :: Foldable t => t a -> [a]
to_list a = foldr (:) [] a

