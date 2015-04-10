import System.IO
import Data.Array

myBinomial :: Int -> Array Int Int
myBinomial n 
	| n == 0 	= first_array
	| otherwise	= calculateCoeffitients n 0 first_array
		where
			first_array = array (0, 0) [(i, 1) | i <- [0..0]]

calculateCoeffitients :: Int -> Int -> Array Int Int -> Array Int Int
calculateCoeffitients n k previous = 
	if n == k
		then previous
		else calculateCoeffitients n (k+1) (generateCoeffitientsFrom previous (k+2))

generateCoeffitientsFrom :: Array Int Int -> Int -> Array Int Int
generateCoeffitientsFrom previous length = array (0, (length-1)) [(i, ((safeGetCoeffitient previous (i-1) (length-1))+(safeGetCoeffitient previous i (length-1)))) | i <- [0..(length-1)]]

safeGetCoeffitient :: Array Int Int -> Int -> Int -> Int
safeGetCoeffitient arr index length = 
	if index < 0 || index >= length
		then 0
		else arr!index

main = do
	n <- readLn
	print (myBinomial n)