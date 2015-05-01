import TypedLambda

type_to_string :: Type -> String
type_to_string t = case t of
	TpInvalid -> "!ERROR!"
	TpBool -> "BOOL"
	TpNamed name -> name
	TpArr t1 t2 -> (type_to_string t1) ++ " -> " ++ (type_to_string t2)

main = do
	let term = TmApplication (TmAbstraction "k" (TpNamed "K1") (TmApplication (TmAbstraction "x" (TpNamed "X1") (TmAbstraction "y" (TpNamed "Y1") (TmTypedVariable 1 (TpNamed "X1")))) (TmTypedVariable (-1) (TpNamed "X1")))) (TmTypedVariable (-2) (TpNamed "K1"))
	putStrLn (type_to_string (typeof term))
