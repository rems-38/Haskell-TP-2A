my_map :: (a -> b) -> [a] -> [b]
my_map f [] = []
my_map f (x:xs) = f x : my_map f xs

my_filter :: (a -> Bool) -> [a] -> [a]
my_filter f [] = []
my_filter f (x:xs) = if f x == True then x : my_filter f xs else my_filter f xs

left :: (a, b) -> a
left (x, _) = x

right :: (a, b) -> b
right (_, x) = x

my_unzip :: [(a, b)] -> ([a], [b])
my_unzip [] = ([], [])
my_unzip ((x1, x2):xs) = (x1 : left (my_unzip xs), x2 : right (my_unzip xs))