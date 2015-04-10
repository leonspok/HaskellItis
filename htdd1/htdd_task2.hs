module YesItCanFlyAirlines where

-- problem A
data DinnerOrder = Chicken | Pasta | EmptyOrder deriving (Show, Eq, Ord)
-- interp. possible dishes passenger may order

func_with_dinner_order :: DinnerOrder -> a
func_with_dinner_order dish = case dish of
	Chicken -> undefined
	Pasta -> undefined
	EmptyOrder -> undefined


-- problem B
dinner_order_to_msg :: DinnerOrder -> String
dinner_order_to_msg dish = case dish of
	Chicken -> "The passenger ordered chicken."
	Pasta -> "The passenger ordered pasta."
	EmptyOrder -> "The passenger ordered nothing."