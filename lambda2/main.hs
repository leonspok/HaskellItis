import TypedLambda

type_to_string :: Type -> String
type_to_string t = case t of
	TpInvalid -> "!ERROR!"
	TpBool -> "BOOL"
	TpNamed name -> name
	TpArr t1 t2 -> (type_to_string t1) ++ " -> " ++ (type_to_string t2)

main = do
	let term = TmApplication (TmAbstraction "k" (TpNamed "K1") (TmApplication (TmAbstraction "x" (TpNamed "T2") (TmAbstraction "y" (TpNamed "T1") (TmVariable 1))) (TmVariable 0))) (TmTypedVariable (TpNamed "K1"))
	let context = [(TpNamed "T2"), (TpNamed "T3")]
	putStrLn (type_to_string (typeof context term))
