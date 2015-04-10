import System.IO

myFibonacсi :: Int -> Int
myFibonacсi n 
	| n == 0 	= 0
	| n == 1 	= 1
	| otherwise = (myFibonacсi (n-1)) + (myFibonacсi (n-2))

main = do
	n <- readLn
	print (myFibonacсi n)