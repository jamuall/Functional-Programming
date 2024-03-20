module pc4
import StdEnv

digits :: Int [Int] -> [Int]
digits 0 x = x
digits n x = digits (n/10) [n rem 10 : x]

//Start = digits 12321 []
// digits 12321 []
// digits 1232 [1:[]]
// digits 123 [2:[1:[]]]
// digits 12 [3:[2:[1:[]]]]
// digits 1  [2:[3:[2:[1:[]]]]]
// digits 0  [1:[2:[3:[2:[1:[]]]]]]
// digits 0 [1,2,3,2,1]
// [1,2,3,2,1] with correct order

digitsToList :: Int -> [Int]
digitsToList x
| x < 10 = [x]
= [x rem 10: digitsToList (x/10)]
// Start = digitsToList 655 [5,5,6]

eqHalf :: Int -> Bool
eqHalf x = take half list == drop half list
where 
	list = (digitsToList x)
	half = (length list) / 2
	

/*1-
* Given a positive integer number, check if the given number is a Disarium number or not.
* A Disarium number is a number defined by the following process :
* Sum of its digits powered with their respective position is equal to the original number.
* For example 135 is a Disarium number :
* As 1^1+3^2+5^3 = 135
*/
isDisariumNumAux :: Int [Int] -> [Int]
isDisariumNumAux _ [] = []
isDisariumNumAux i [x:xs] = [x ^ i : isDisariumNumAux (i-1) xs]

//Start = isDisariumNumAux 5 [1, 2, 3, 4, 5]

// Start = isDisariumNum 135 // True
// Start = isDisariumNum 598 // True
// Start = isDisariumNum 518 // True
// Start = isDisariumNum 220 // False
// Start = isDisariumNum 110 // False



/*2-
    * Given a list of positive integer numbers, return a list contains the 'Harshad' numbers in the given list.
    * a harshad number is an integer that is divisible by the sum of its digits when written in that base.
    * Examples:
                1-  Number 200 is a Harshad Number because the sum of digits 2 , 0 and 0 is 2 and 200 is divisible by 2. 
                2- Number 171 is a Harshad Number because the sum of digits 1 , 7 and 1 is 9 and 171 is divisible by 9.
*/

harshadNums :: [Int] -> [Int]
harshadNums [] = []
harshadNums [x:xs]
| x rem (sum (digitsToList x)) == 0 = [x: harshadNums xs]
=harshadNums xs

//Start = harshadNums ([8, 9, 10, 12, 18, 20, 21, 24, 27, 30] ++ [13..17]) //   [8, 9, 10, 12, 18, 20, 21, 24, 27, 30] 
//Start = harshadNums ([31..35] ++ [36, 17,40, 42, 45, 13, 48, 50, 54, 11, 60, 63]) // [36, 40, 42, 45, 48, 50, 54, 60, 63]
//Start = harshadNums [] // []


/*3- 
    *Given a list of integer numbers, return all the LEADERS in the list.
    A number is leader if it is greater than all the elements to its right side.
    Example:
       15 [10,9,14,23,15,0,9] -> [23,15,9]
        23 is greater than all the numbers to its right (15,0,9).  
        15 is greater than all the numbers to its right (0,9).  
        9 there is no numbers in its right.  
*/
leadersAux :: Int [Int] -> Bool
leadersAux x list
| list == [] = True
| x > (hd list) = leadersAux x (tl list)
= False
//Start = leadersAux 5 [4, 3, 2, 1]


leaders :: [Int] -> [Int]
leaders [] = []
leaders x
| leadersAux (hd x) (tl x) == True = [ hd x : leaders (tl x)]
= leaders (tl x)





//Start = leaders  [10,9,14,23,15,0,9] // [23,15,9]
// Start = leaders  [1..10] // [10]
// Start = leaders  [10,9..1] // [10,9,8,7,6,5,4,3,2,1]
// Start = leaders  [7,8,10,9,5,3,6,4] // [10,9,6,4]
// Start = leaders  [] // []



