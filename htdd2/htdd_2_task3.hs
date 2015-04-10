module AlternateList where

data AlternateList a b = Nil | AFirst a (AlternateList b a) | BFirst b (AlternateList a b)
-- interp. list with in which element of type b follows element of type a and in the other way

empty_list :: AlternateList a b
empty_list = Nil

length_of_list :: AlternateList a b -> Int
length_of_list list = case list of
	Nil -> 0
	AFirst _ tail -> 1 + (length_of_list tail)
	BFirst _ tail -> 1 + (length_of_list tail)

dmap :: AlternateList a b -> (a -> x) -> (b -> y) -> AlternateList x y
dmap list fa fb = case list of
	Nil -> Nil
	AFirst value tail -> AFirst (fa value) (dmap tail fb fa)
	BFirst value tail -> BFirst (fb value) (dmap tail fa fb)