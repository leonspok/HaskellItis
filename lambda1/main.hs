import Lambda

convertMaybeTermToTerm :: Maybe Term -> Term
convertMaybeTermToTerm t = case t of
	Just term -> term

main = do
	let tru = (TmAbstraction "x" (TmAbstraction "y" (TmVariable 0)))
	-- evaluation of ((tru x) y) should always return x

	let fls = (TmAbstraction "x" (TmAbstraction "y" (TmVariable 1)))
	-- evaluation of ((tru x) y) should always give you y


	let if_expression = (TmAbstraction "a" (TmAbstraction "b" (TmAbstraction "c" (TmApplication (TmApplication (TmVariable 0) (TmVariable 1)) (TmVariable 2)))))
	-- evaluation of (((if_expression tru) then) else) should return then
	-- otherwise evaluation of (((if_expression fls) then) else) should return else

	let not = (TmAbstraction "t" (TmApplication (TmApplication (TmVariable 0) (term_shift 1 fls)) (term_shift 1 tru)))
	-- evaluation of (not tru) should return fls
	-- evaluation of (not fls) should return tru
	-- we perform shift operation to terms tru and fls because they are inside the abstraction, so we should shift all De Bruijin indexes

	let term = TmApplication (TmApplication (TmApplication if_expression (TmApplication not tru)) (TmNamedVariable "then")) (TmNamedVariable "else")
	-- this term should return term "else" because the condition isn't true (not tru)
	putStrLn (print_term term)
	putStrLn ("Result: " ++ print_maybe_term (evaluate term))
