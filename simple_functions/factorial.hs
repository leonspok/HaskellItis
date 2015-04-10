import System.IO

myFactorial :: Int -> Int
myFactorial x 
	| x == 0 	= 0
	| x == 1 	= 1
	| otherwise = x * (myFactorial (x-1))

main = do
	n <- readLn
	print (myFactorial n)