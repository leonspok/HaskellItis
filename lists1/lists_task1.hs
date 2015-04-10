
map_regular :: (a -> b) -> [a] -> [b]
map_regular _ [] = []
map_regular f (x:xs) = (f x):(map_regular f xs)

map_foldl :: (a -> b) -> [a] -> [b]
map_foldl f list = foldl (\tmpList x -> tmpList ++ [(f x)]) [] list

map_foldr :: (a -> b) -> [a] -> [b]
map_foldr f list = foldr (\x tmpList -> (f x):tmpList) [] list

main = do
	print (map_regular my_func [1, 2, 3, 4])
	print (map_foldl my_func [2, 4, 6, 8])
	print (map_foldr my_func [3, 6, 9, 12])
		where
			my_func :: Int -> Int
			my_func x = x*x