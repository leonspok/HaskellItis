module Lambda where

import Data.Maybe

-- Data Types

type DeBruijnIndex = Int
-- interp. as De Bruijin index for term of type "variable"

first_index, second_index :: DeBruijnIndex
first_index = 0
second_index = 1

fn_for_debruijin_index :: Int -> a
fn_for_debruijin_index x = undefined x



type AbstractionArgumentName = String
-- interp. as name of linked variable for term of type "abstraction:

name_a :: AbstractionArgumentName
name_a = "a"

fn_for_abstraction_argument_name :: AbstractionArgumentName -> a
fn_for_abstraction_argument_name x = undefined x



type VariableName = String
-- interp. as name of term of type "variable"

name_t :: VariableName
name_t = "a"

fn_for_variable_name :: VariableName -> a
fn_for_variable_name x = undefined x



data Term = TmVariable DeBruijnIndex |
			TmNamedVariable VariableName |
			TmAbstraction AbstractionArgumentName Term |
			TmApplication Term Term
-- interp. as term of lambda calculus
-- TmVariable - is a linked variable used inside term of type "abstraction"
-- TmNamedVariable - is a free variable
-- TmAbstraction - is a term for abstract operation
-- TmApplication - is a term for application operation

truChurch, flsChurch :: Term
truChurch = (TmAbstraction "x" (TmAbstraction "y" (TmVariable 0)))
flsChurch = (TmAbstraction "x" (TmAbstraction "y" (TmVariable 1)))

fn_for_term :: Term -> a
fn_for_term term = case term of
	TmVariable index -> undefined index
	TmNamedVariable name -> undefined name
	TmAbstraction name term' -> undefined name term'
	TmApplication term1 term2 -> undefined term1 term2



-- Functions

print_term :: Term -> String
print_term t = print_in_context t [] 
-- this function is a wrapper for recursive function print_in_context

print_maybe_term :: Maybe Term -> String
print_maybe_term t = case t of
	Just term -> print_term term
	Nothing -> "Nothing"
-- this function is used just for printing Maybe Term as Term



type Context = [String]
-- interp. variables names context

ctx :: Context
ctx = ["x", "y"]

fn_for_context :: Context -> a
fn_for_context [] = undefined
fn_for_context (x:xs) = fn_for_context (xs)



pick_fresh_name :: Context -> AbstractionArgumentName -> (Context, AbstractionArgumentName)
pick_fresh_name ctx name
	| elem name ctx 	= pick_fresh_name ctx (name ++ "'")
	| otherwise 		= ((ctx ++ [name]), name)
-- this function transforms names of variables if they are already linked inside an abstraction

print_in_context :: Term -> Context -> String
print_in_context term context = case term of
	TmAbstraction name innerTerm -> 
		let (newContext, newName) = pick_fresh_name context name
		in "(\\" ++ newName ++ ". " ++ (print_in_context innerTerm newContext) ++ ")"
	TmApplication term1 term2 -> "(" ++ (print_in_context term1 context) ++ " " ++ (print_in_context term2 context) ++ ")"
	TmVariable index -> 
		if (index < (length context)) 
			then context !! index
			else (show index)
	TmNamedVariable s -> s
-- this function converts term with current context into string 

term_shift :: Int -> Term -> Term
term_shift d term = 
	walk 0 term
		where
			walk :: DeBruijnIndex -> Term -> Term
			walk c term = case term of
				TmVariable index -> TmVariable (index+d)
				TmAbstraction name term1 -> TmAbstraction name (walk (c+1) term1)
				TmApplication term1 term2 -> TmApplication (walk c term1) (walk c term2)
				TmNamedVariable s -> TmNamedVariable s
-- this function shifts all variables DeBruijin indexes by d

term_substitute :: DeBruijnIndex -> Term -> Term -> Term
term_substitute index term_substitute_with term = 
	walk 0 term
		where
			walk :: DeBruijnIndex -> Term -> Term
			walk c term = case term of
				TmVariable varIndex ->
					if varIndex == index
						then term_shift c term_substitute_with
					else TmVariable varIndex
				TmAbstraction name term1 -> TmAbstraction name (walk (c+1) term1)
				TmApplication term1 term2 -> TmApplication (walk c term1) (walk c term2)
				TmNamedVariable s -> TmNamedVariable s
-- this function replaces all veriables with index <index> inside term "term" with term "term_substitute_with"

term_substituteTop :: Term -> Term -> Term
term_substituteTop term_substitute_with term = term_shift (-1) (term_substitute 0 (term_shift 1 term_substitute_with) term)
-- this function represnts beta-reduction

is_abstraction :: Term -> Bool
is_abstraction term = case term of
	TmAbstraction _ _ -> True
	_ -> False
-- this function returns True is term is an abstraction

evaluate_step :: Term -> Maybe Term
evaluate_step term = case term of
	TmApplication t term2 -> case t of
		TmAbstraction _ term1 -> Just (term_substituteTop term2 term1)
		_ -> 
			if (is_abstraction t) 
				then
					let term2' = evaluate_step term2
					in case term2' of
						Just t' -> Just(TmApplication t t')
						Nothing -> Nothing
			else 
				let term1' = evaluate_step t
				in case term1' of
					Just t' -> Just (TmApplication t' term2)
					Nothing -> Nothing
	_ -> Nothing
-- this function makes one step in term evaluating
-- if there is some problems function will return Nothing

evaluate :: Term -> Maybe Term
evaluate term = 
	let term' = evaluate_step term
	in case term' of
		Just t -> evaluate t
		Nothing -> Just term
-- this function evaluates whole term
-- if there is some problems function will return Nothing



