planetes :: [(Double, String)]
planetes = [(0.39, "Mercure"), (0.72, "Venus"), (1.00, "Terre"), (1.52, "Mars"), (5.20, "Jupiter"), (9.54, "Saturne"), (19.2, "Uranus"), (30.1, "Neptune")]

-- elemIndex :: (Eq a) => a -> [a] -> Maybe Int
-- elemIndex n l = elemIndex_aux n l 0

-- elemIndex_aux :: (Eq a) => a -> [a] -> Int -> Maybe Int
-- elemIndex_aux _ [] _ = Nothing
-- elemIndex_aux n (x:xs) r | x == n = Just r
--                          | otherwise = elemIndex_aux n xs (1+r)

-- findIndex :: Int -> [a] -> Maybe a
-- findIndex (-1) l = Nothing
-- findIndex n l = findIndex_aux n l 0

-- findIndex_aux :: Int -> [a] -> Int -> Maybe a
-- findIndex_aux _ [] _ = Nothing
-- findIndex_aux n (x:xs) r | n == r = Just x
--                          | otherwise = findIndex_aux n xs (1+r)

-- unMaybe :: Maybe Int -> Int
-- unMaybe Nothing = (-1) -- pour gérer l'exception dans le findIndex et qu'on puisse retourner Nothing (oui c'est pas beau)
-- unMaybe (Just x) = x

-- fst_pl = map fst planetes
-- snd_pl = map snd planetes

-- distance :: String -> Maybe Double
-- distance name = findIndex (unMaybe $ index) fst_pl
--                where index = elemIndex name snd_pl
                     
-- suivante :: Double -> Maybe String
-- suivante d = findIndex (unMaybe $ elemIndex index fst_pl) snd_pl
--             where index = head $ filter (>d) fst_pl

distance_aux :: String -> [(Double, String)] -> Maybe Double
distance_aux _ [] = Nothing
distance_aux name ((d,s):r) | name == s = Just d
                            | otherwise = distance_aux name r

distance :: String -> Maybe Double
distance name = distance_aux name planetes

suivante_aux :: Double -> [(Double, String)] -> [(Double, String)]
suivante_aux dis ((d,s):r) = filter (\(a,_) -> dis < a) planetes

suivante :: Double -> Maybe String
suivante d | s_aux == [] = Nothing
           | otherwise = Just $ snd $ head $ s_aux
           where s_aux = suivante_aux d planetes

ua_vers_km :: Double -> Double
ua_vers_km = (*) 1.496e+8

unMaybe :: Maybe a -> a
unMaybe (Just x) = x

distance_km_1 :: String -> Maybe Double
distance_km_1 name | distance name == Nothing = Nothing
                   | otherwise = Just $ ua_vers_km (unMaybe $ distance name)

isNoth :: Maybe a -> Bool
isNoth Nothing = True
isNoth (Just _) = False

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

fmap_Maybe :: (a -> b) -> Maybe a -> Maybe b
fmap_Maybe _ Nothing = Nothing
fmap_Maybe f x | isNoth x = Nothing
               | otherwise = Just $ f (unMaybe x)

-- distance_km_2 :: String -> Maybe Double
-- distance_km_2 name = fmap_Maybe ua_vers_km (distance name)

distance_km_2 :: String -> Maybe Double
distance_km_2 name = ua_vers_km <$> (distance name)

distance_suivante_1 :: String -> Maybe Double
distance_suivante_1 name = case distance name of
                            Nothing -> Nothing
                            Just d -> case suivante d of 
                                       Nothing -> Nothing
                                       Just n -> distance n 

bind_Maybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bind_Maybe Nothing _ = Nothing
bind_Maybe (Just x) f = f x

-- distance_suivante_2 :: String -> Maybe Double
-- distance_suivante_2 name = bind_Maybe (bind_Maybe (distance name) suivante) distance

distance_suivante_2 :: String -> Maybe Double
distance_suivante_2 name = distance name >>= suivante >>= distance

