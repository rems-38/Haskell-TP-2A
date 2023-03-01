puissance :: (Eq t, Num t, Num p) => p -> t -> p
puissance _ 0 = 1
puissance a n = a * puissance a (n-1)

fact :: (Eq p, Num p) => p -> p
fact 0 = 1
fact n = n * (fact (n-1))

somme_carres :: (Num p, Eq p) => p -> p
somme_carres 0 = 0
somme_carres n = n^2 + (somme_carres (n-1))

fibonacci :: (Eq a, Num a, Num p) => a -> p
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci 2 = 1
fibonacci n = (fibonacci (n-1)) + (fibonacci (n-2))

fastExp :: (Integral a1, Num a2) => a2 -> a1 -> a2
fastExp _ 0 = 1
fastExp a n = if n `mod` 2 == 0 then (fastExp a (n `div` 2)) ^ 2 else ((fastExp a (n `div` 2)) ^ 2) * a
