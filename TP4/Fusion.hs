halve :: [Int] -> ([Int], [Int])
halve xs = halve_aux xs [] []

halve_aux :: [Int] -> [Int] -> [Int] -> ([Int], [Int])
halve_aux [] ys zs = (ys, zs)
halve_aux (x:xs) ys zs = halve_aux xs zs (x:ys)

combine :: [Int] -> [Int] -> [Int]
combine [] ys = ys
combine xs [] = xs 
combine (x:xs) (y:ys) | x <= y = x : (combine xs (y:ys))
                      | otherwise = y : (combine (x:xs) ys)

tri_fusion :: [Int] -> [Int]
tri_fusion [] = []
tri_fusion [x] = [x]
tri_fusion xs = fusion (tri_fusion (fst $ halve xs), tri_fusion (snd $ halve xs))

fusion :: ([Int], [Int]) -> [Int]
fusion (a, b) = combine a b