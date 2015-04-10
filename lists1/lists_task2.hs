
-- filter
filter_regular :: (a -> Bool) -> [a] -> [a]
filter_regular _ [] = []
filter_regular f (x:xs) = case (f x) of
							True -> x:(filter_regular f xs)
							False -> (filter_regular f xs)

filter_foldl :: (a -> Bool) -> [a] -> [a]
filter_foldl f list = foldl (\tmpList x -> if (f x) then tmpList ++ [x] else tmpList) [] list

filter_foldr :: (a -> Bool) -> [a] -> [a]
filter_foldr f list = foldr (\x tmpList -> if (f x) then x:tmpList else tmpList) [] list


-- concat
concat_regular :: [[a]] -> [a]
concat_regular [] = []
concat_regular (x:xs) = x ++ (concat_regular xs)

concat_foldl :: [[a]] -> [a]
concat_foldl list = foldl (\tmpList x -> tmpList ++ x) [] list

concat_foldr :: [[a]] -> [a]
concat_foldr list = foldr (\x tmpList -> x ++ tmpList) [] list


-- concatMap
concat_map_regular :: (a -> [b]) -> [a] -> [b]
concat_map_regular _ [] = []
concat_map_regular f (x:xs) = (f x) ++ (concat_map_regular f xs)

concat_map_foldl :: (a -> [b]) -> [a] -> [b]
concat_map_foldl f list = foldl (\tmpList x -> tmpList ++ (f x)) [] list

concat_map_foldr :: (a -> [b]) -> [a] -> [b]
concat_map_foldr f list = foldr (\x tmpList -> (f x) ++ tmpList) [] list

main = do
	print(filter_regular my_filter_func [1, 2, 3, 4, 5, 6])
	print(filter_foldl my_filter_func [3, 4, 5, 6, 7, 8])
	print(filter_foldr my_filter_func [5, 6, 7, 8, 9, 10])
	print(concat_regular [[1, 2], [3, 4], [5, 6, 7]])
	print(concat_foldl [[1, 2], [3, 4], [5, 6, 7]])
	print(concat_foldr [[1, 2], [3, 4], [5, 6, 7]])
	print(concat_map_regular my_map_func [1, 2, 3])
	print(concat_map_foldl my_map_func [1, 2, 3])
	print(concat_map_foldr my_map_func [1, 2, 3])
		where
			my_filter_func :: (Int -> Bool)
			my_filter_func x 
				| (mod x 2) == 0 	= True
				| otherwise			= False

			my_map_func :: Int -> [String]
			my_map_func x = [("pow1:" ++ (show x)), ("pow2:" ++ (show (x*x))), ("pow3:" ++ (show (x*x*x)))]


