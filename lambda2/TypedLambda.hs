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



data Term = TmTypedVariable DeBruijnIndex Type |
			TmAbstraction AbstractionArgumentName Type Term |
			TmApplication Term Term
-- interp. as term of lambda calculus
-- TmVariable - is a linked variable used inside term of type "abstraction"
-- TmTypedVariable - is a free variable with a specific type
-- TmAbstraction - is a term for abstract operation
-- TmApplication - is a term for application operation

simple_abstraction :: Term
simple_abstraction = TmAbstraction "x" type_of_true (TmTypedVariable 0 (TpNamed "X"))

fn_for_term :: Term -> a
fn_for_term term = case term of
	TmTypedVariable index t -> undefined index t
	TmAbstraction name t term' -> undefined name t term'
	TmApplication term1 term2 -> undefined term1 term2



-- Functions

typeof :: Term -> Type
typeof term = case term of
	TmTypedVariable _ t -> t
	TmAbstraction arg argType t -> 
		let bodyType = typeof t in
		TpArr argType bodyType
	TmApplication t1 t2 ->
		let t1Type = typeof t1 in
		let t2Type = typeof t2 in
		case t1Type of
			TpArr t11Type t12Type ->
				if t2Type == t11Type
					then t12Type
					else TpInvalid
			_ -> TpInvalid



