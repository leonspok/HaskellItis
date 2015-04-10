import System.IO

myAckermann :: Int -> Int -> Int
myAckermann m n = 
	if m == 0
		then n+1
		else if m > 0 && n == 0
			then myAckermann (m-1) 1
			else if m > 0 && n > 0
				then myAckermann (m-1) (myAckermann m (n-1))
				else 0

main = do
	n <- readLn
	m <- readLn
	print (myAckermann m n)