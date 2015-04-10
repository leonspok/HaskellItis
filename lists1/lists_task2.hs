
-- filter
filter_regular :: (a -> Bool) -> [a] -> [a]
filter_regular _ [] = []
filter_regular f (x:xs) = case (f x) of
							True -> x:(filter_regular f xs)
							False -> (filter_regular f xs)

filter_foldl :: (a -> Bool) -> [a] -> [a]
filter_foldl _ [] = []
filter_foldl f list = foldl (\tmpList x -> if (f x) then tmpList ++ [x] else tmpList) [] list

filter_foldr :: (a -> Bool) -> [a] -> [a]
filter_foldr _ [] = []
filter_foldr f list = foldr (\x tmpList -> if (f x) then x:tmpList else tmpList) [] list

main = do
	print(filter_regular my_filter_func [1, 2, 3, 4, 5, 6])
	print(filter_foldl my_filter_func [3, 4, 5, 6, 7, 8])
	print(filter_foldr my_filter_func [5, 6, 7, 8, 9, 10])
		where
			my_filter_func :: (Int -> Bool)
			my_filter_func x 
				| (mod x 2) == 0 	= True
				| otherwise			= False