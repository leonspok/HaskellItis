module Oatmeal where

-- problem A
type OatmealTemp = Int
-- interp. as oatmeal temperature

perfect_oatmeal_temp, lowest_stove_temp, highest_stove_temp :: OatmealTemp
perfect_oatmeal_temp = 10
lowest_stove_temp = 0
highest_stove_temp = 20

func_with_oatmeal :: OatmealTemp -> a
func_with_oatmeal t | t >= 0 || t <= 20 = undefined


-- problem B
data Adjustment = TurnLeft | TurnRight | DoNothing deriving (Show, Eq, Ord)
-- interpr. stove adjustment action type

func_with_adjustment :: Adjustment -> a
func_with_adjustment adjustment = case adjustment of
	TurnLeft -> undefined
	TurnRight -> undefined
	DoNothing -> undefined


-- problem C
oatmeal_temp_to_adjustment :: OatmealTemp -> Adjustment
oatmeal_temp_to_adjustment t 
	| t >= 0 || t <= 20 	= temp_to_adjustment t
		where
			temp_to_adjustment x 
				| x < perfect_oatmeal_temp 	= TurnRight
				| x == perfect_oatmeal_temp = DoNothing
				| x > perfect_oatmeal_temp 	= TurnLeft


