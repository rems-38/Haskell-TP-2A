--z :: Integer -> Bool
z = (>=) 3

--app :: (Integer -> Bool) -> [Bool]
app f = map f [1..5]

echange :: (a -> b -> c) -> (b -> a -> c)
echange f x y = f y x

mod2 :: Integer -> Integer
mod2 = echange mod 2

{- Réponse questions :
1. La fonction z calcule si la valeur d'entrée est supérieur ou à trois
2. On peut supposer que `map z [1..5]` va renvoyer `[True, True, True, False, False]`. En vérifiant, c'est bien le cas
3. app :: (Interger -> Bool) -> [Bool]
   app z :: [Bool]
4. Si on souhaite obtenir le même resultat qu'en question 2, on peut utiliser `app z`
5. a. `app succ` nous renvoie bien [2..6]
   b. `app (max 4)` fonctionne pour ce qu'on souhaite, cela renvoie bien [4,4,4,4,5]


-}