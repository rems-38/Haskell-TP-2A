somme_liste :: [Int] -> Int
somme_liste [] = 0
somme_liste (x:xs) = x + (somme_liste xs)

acc_ints :: (Int -> Int -> Int) -> Int -> [Int] -> Int
acc_ints op neutre [] = neutre
acc_ints op neutre (x:xs) = op x (acc_ints op neutre xs) 

max_liste :: [Int] -> Int
max_liste list = acc_ints max 0 list

-- acc :: (a -> a -> a) -> a -> [a] -> a
acc op neutre [] = neutre
acc op neutre (x:xs) = op x (acc op neutre xs)

my_foldl :: (b -> a -> b) -> b -> [a] -> b
my_foldl op neutre [] = neutre
my_foldl op neutre (x:xs) = op (my_foldl op neutre xs) x 

my_foldr :: (a -> b -> b) -> b -> [a] -> b
my_foldr op neutre [] = neutre
my_foldr op neutre (x:xs) = op x (my_foldr op neutre xs)

join :: [String] -> String
join [] = ""
join (x:xs) = x ++ " " ++ (join xs)

