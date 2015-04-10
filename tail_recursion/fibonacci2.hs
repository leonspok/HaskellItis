import System.IO

myFibonacсi :: Int -> Int
myFibonacсi n = calculateFibonacci n (-1) 0 1

calculateFibonacci :: Int -> Int-> Int -> Int -> Int
calculateFibonacci n k a b = 
	if k == n
		then b
		else if k == n-1
			then a
			else calculateFibonacci n (k+1) b (a+b)


main = do
	n <- readLn
	print (myFibonacсi n)