module TypedLambda where

-- Data types

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



type TypeName = String
-- interp. as a name of specific type

type_a :: TypeName
type_a = "A"

fn_for_type_name :: TypeName -> a
fn_for_type_name x = undefined x



data Type = TpInvalid | 
			TpBool | 
			TpNamed TypeName |
			TpArr Type Type deriving (Show,Eq)
-- interp. as Type of a term

type_of_true :: Type
type_of_true = TpBool

fn_for_type :: Type -> a
fn_for_type t = case t of
	TpInvalid -> undefined
	TpBool -> undefined
	TpNamed tName -> undefined tName
	TpArr t1 t2 -> undefined t1 t2



type Context = [Type]
-- interp. as context with variable types

one_element_context :: Context
one_element_context = [type_of_true]

fn_for_context :: Context -> a
fn_for_context ctx = case ctx of
	x:xs -> undefined x (fn_for_context xs)
	_ -> undefined



data Term = TmVariable DeBruijnIndex |
			TmTypedVariable Type |
			TmAbstraction AbstractionArgumentName Type Term |
			TmApplication Term Term
-- interp. as term of lambda calculus
-- TmVariable - is a linked variable used inside term of type "abstraction"
-- TmTypedVariable - is a free variable with a specific type
-- TmAbstraction - is a term for abstract operation
-- TmApplication - is a term for application operation

simple_abstraction :: Term
simple_abstraction = TmAbstraction "x" type_of_true (TmVariable 0)

fn_for_term :: Term -> a
fn_for_term term = case term of
	TmVariable index -> undefined index
	TmTypedVariable t -> undefined t
	TmAbstraction name t term' -> undefined name t term'
	TmApplication term1 term2 -> undefined term1 term2



-- Functions

get_type_from_context :: Context -> DeBruijnIndex -> Type
get_type_from_context ctx index = 
	if index < length ctx
		then ctx !! index
		else TpInvalid

add_binding :: Context -> Type -> Context
add_binding ctx t = ctx ++ [t]

typeof :: Context -> Term -> Type
typeof ctx term = case term of
	TmVariable index -> get_type_from_context ctx index
	TmTypedVariable t -> t
	TmAbstraction arg argType t -> 
		let ctx' = add_binding ctx argType in
		let bodyType = typeof ctx' t in
		TpArr argType bodyType
	TmApplication t1 t2 ->
		let t1Type = typeof ctx t1 in
		let t2Type = typeof ctx t2 in
		case t1Type of
			TpArr t11Type t12Type ->
				if t2Type == t11Type
					then t12Type
					else TpInvalid
			_ -> TpInvalid



