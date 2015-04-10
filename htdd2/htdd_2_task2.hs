module BinaryTree where

data BinaryTree t = Nil | Cons t (BinaryTree t) (BinaryTree t) deriving(Show, Eq)
-- interp. binary search tree containing elements of type Int

empty_tree :: BinaryTree t
empty_tree = Nil

func_with_binary_search_tree :: BinaryTree t -> a
func_with_binary_search_tree tree = case tree of
	Nil -> undefined
	Cons value leftElement rightElement -> undefined value (func_with_binary_search_tree leftElement) (func_with_binary_search_tree rightElement)

height_of_tree :: BinaryTree t -> Int
height_of_tree tree = case tree of
	Nil -> 0
	Cons _ leftElement rightElement -> max (height_of_tree leftElement) (height_of_tree rightElement)

tmap :: BinaryTree a -> (a -> b) -> BinaryTree b
tmap tree f = case tree of 
	Nil -> Nil
	Cons value leftElement rightElement -> Cons (f value) (tmap leftElement f) (tmap rightElement f)