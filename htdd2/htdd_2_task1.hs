module BinarySearchTree where

data BinarySearchTree = Nil | Cons Int BinarySearchTree BinarySearchTree deriving(Show, Eq)
-- interp. binary search tree containing elements of type Int

empty_tree :: BinarySearchTree
empty_tree = Nil

tree_leaf :: BinarySearchTree
tree_leaf = Cons 1 empty_tree empty_tree

func_with_binary_search_tree :: BinarySearchTree -> a
func_with_binary_search_tree tree = case tree of
	Nil -> undefined
	Cons value leftElement rightElement -> undefined value (func_with_binary_search_tree leftElement) (func_with_binary_search_tree rightElement)

height_of_tree :: BinarySearchTree -> Int
height_of_tree tree = case tree of
	Nil -> 0
	Cons _ leftElement rightElement -> max (height_of_tree leftElement) (height_of_tree rightElement)

sum_of_tree :: BinarySearchTree -> Int
sum_of_tree tree = case tree of
	Nil -> 0
	Cons value leftElement rightElement -> value + (sum_of_tree leftElement) + (sum_of_tree rightElement)

search_element_in_binary_tree :: Int -> BinarySearchTree -> BinarySearchTree
search_element_in_binary_tree valueToFound tree = case tree of
	Nil -> Nil
	Cons value leftElement rightElement -> 
		if valueToFound == value
			then tree
		else if valueToFound < value
			then leftElement
		else rightElement