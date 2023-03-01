data Tree a = Leaf a | Node a [Tree a] deriving Show 

arbre_TP :: Tree String
arbre_TP = Node "CS222" [Node "TP1" [Leaf "CS222_TP1.pdf", Leaf "applis.hs", Leaf "rec.hs", Leaf "test_rec.ghci"], Node "TP2" [Leaf "CS222_TP2.pdf", Leaf "chaines.hs", Leaf "tarot.hs"], Node "TP3" []]

orElse :: Maybe a -> Maybe a -> Maybe a
orElse (Just x) _ = Just x
orElse Nothing y = y

descendre :: Tree String -> String -> Maybe (Tree String)
descendre (Leaf _) _ = Nothing
descendre a@(Node arbre subArbre) input
    | input == arbre = Just a
    | otherwise = foldr orElse Nothing (map (\f -> descendre f input) subArbre)

-- pas sur de descendre (cas de base ET peut etre un peu complexe)
