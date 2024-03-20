module PT2
import StdEnv 

/* Write a function that sums the odd digits of a given positive integer number*/
digitsToList :: Int -> [Int]
digitsToList x
| x < 10 = [x]
= [x rem 10] ++ digitsToList (x/10)

sumOddDigits :: Int -> Int 
sumOddDigits x 
| x < 10 && isOdd x = x
| x < 10 && isEven x = 0
= sum(filter isOdd (digitsToList x))


//Start = sumOddDigits 123 // 4
//Start = sumOddDigits 223456 // 8
//Start = sumOddDigits 5555 // 20
//Start = sumOddDigits 1 // 1
//Start = sumOddDigits 2 // 0
