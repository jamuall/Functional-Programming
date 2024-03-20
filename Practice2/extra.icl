module extra
import StdEnv

/*
    Given a three digit number, check 
    if the middle digit of this number is odd or even;
    E.g:
        154  : the middile digit is 5 and it is odd => output should be "Odd"
        545 :  the middile digit is 4 and it is even => output should be "Even"
*/

check :: Int -> String 
check n
|((n / 10) rem 10) rem 2 == 0 = "Even"
= "Odd"
//Start = check 154 // "Odd"
//Start = check 222 // "Even"
//Start = check 111 // "Odd"
//Start = check 545 // "Even"


/*-1 
    * Roman numerals are represented by seven different symbols: I, V, X, L, C, D and M.
     Symbol       Value
        I             1
        V             5
        X             10
        L             50
        C             100
        D             500
        M             1000
    
    * Write the function "getValue" which gets a symbol and returns the represented value of that symbol according to the table above.
    * If the given symbol is not in the table above, abort with "Not valid input" 
    * hint:  you can use the abort funciton

    Example for the abort function:
    task: calculate the factorial of a positive number
          if the number is negative, abort with "invalid argument"

          solution:
fact :: Int -> Int
fact x
| x <  0 = abort "invalid argument" // if the value is less than 0 -> abort the program with this message "invalid argument" 
| x == 1 = 1 
= x * fact (x - 1)   

Start = fact 3 // 6 
Start = fact 4 // 24 
Start = fact -1 //  invalid argument" 
          

*/

getValue :: Char -> Int
getValue x 
| x == 'I' = 1
| x == 'V' = 5
| x == 'X' = 10
| x == 'L' = 50
| x == 'C' = 100
| x == 'D' = 500
| x == 'M' = 1000
= abort "Invalid input"


/// Used to test your funciton if it produces the correct answer or not !

// Start = getValue 'I' // 1
// Start = getValue 'V' // 5
// Start = getValue 'X' // 10 
// Start = getValue 'L' // 50
// Start = getValue 'C' // 100
//Start = getValue 'D' // 500 
// Start = getValue 'M' // 1000
// Start = getValue '0' //  "invalid input"


/*2-  
    Create a function which transforms the number of days to years, weeks and days. 
    For example: 375 days = 1 year 1 week 3 days.
    - 1 year = 365 days (Ignoring leap year)

    Hints:
         * you can use the toString function: the funciton  transforms the given input to String (e.g toString 5  ->  "5")
         * don't forget the paranthesis!
*/



// transform :: Int -> String 
// Write your code here ...


// Start = transform 375 // "1 year 1 week 3 days"
// Start = transform 365 // "1 year 0 week 0 days"
// Start = transform 1050 // "2 year 45 week 5 days"
// Start = transform 2500 // "6 year 44 week 2 days"

/*-3
    * The first element of the Collatz series is an arbitrary positive integer. Following elements could be defined recursively:
    * the next element is 3 times the previous element increased by one, if the previous element was odd, otherwise the next
    * element is the half of the previous element.
    * 
    * As an example, starting from 3 as an input, the elements of the Collatz series are
    * 3, 10, 5, 16, 8, 4, 2, 1
    * 
    * According to the Collatz conjecture, this series **always** reaches 1, which is the last element in the previous
    * example, making the length of the Collatz series 8.

    * The function gets the first element of the series as input and should return the length of the Collatz series
*/

// len :: Int -> Int
// Write your code here

// Start = len 3 // 8 
// Start = len 5 // 6   
// Start = len 8 // 4   
// Start = len -8 // "Invalid input"    






// 9

// . Change a character if is small letter to uppercase and vice versa,
// if is a special sign, leave as it is.
ch :: Char -> Char
ch c
| 65 <= (toInt c) && (toInt c) <= 90 = toLower c
| 97 <= (toInt c) && (toInt c) <= 122 = toUpper c
= c

// How to identify wether the letter is capital or small?

 
// Start = ch 'a'  // ('A','Z','a','z','X','%')
// Start = toInt 'A'
// Start = toUpper 'a'


/*10

,	Given an integer, x, calculate the total number of perfect numbers between 0 to that number, [1,x).
	Perfect number is a number which is equal to the sum of its divisor.
	For example: 6 is a perfect number
				divisor of 6 = 1, 2, 3 => sum of its divisor = 6 == 6
				10 is not a perfect number
				divisor of 10 = 1, 2, 5 => sum = 8 != 10
				
	If the given x is 10, then the result is 1 because there is only 1 perfect number between 1 and x.
	
	You are advised to write one or more functions for clear coding. 
*/

//numOfPerfectNumber :: Int -> Int
//numOfPerfectNumber x =

//Start = numOfPerfectNumber 29 	// 2
//Start = numOfPerfectNumber 1000 	// 3
//Start = numOfPerfectNumber 0 		// 0
//Start = numOfPerfectNumber 1 		// 0
//Start = numOfPerfectNumber 10000 	// 4

/*11


, Given an integer, find the sum of the square of each number between 1 and that number.
	
	if the given integer is 5, then 5^2+4^2+3^2+2^2+1^2 = 55
	
*/

//SquareSum :: Int->Int
//SquareSum n

//Start = SquareSum 5 // 55
//Start = SquareSum 0 // 0
//Start = SquareSum 1 // 1
//Start = SquareSum 100 // 338350



//12

//. Compute factorial n recursively, where n! = n*(n-1)! .
factor :: Int -> Int
factor n
| n == 0 = 1
| n > 0 = n * factor (n-1)

//Start = factor 5 // 120
// factor 5
// 5 * factor 4
// 5 * 4 * factor 3
// 5 * 4 * 3 * factor 2
// 5 * 4 * 3 * 2 * factor 1
// 5 * 4 * 3 * 2 * 1 * factor 0
// 5 * 4 * 3 * 2 * 1 * 1
// 120 */

// Start = factor 0


// 13

// . Given two integers, put their digits together like: 123 456 =123456
// countDigits :: Int -> Int
// countDigits x 

//Start = countDigits 456 // 3

// glue :: Int Int -> Int 
// glue a b = a * (10^(countDigits b)) + b

//Start = glue 123000 456 // 123000456
//Start = glue 765 432 // 765432

glue2 :: Int Int -> Int
glue2 a b = toInt  ((toString a) +++ (toString b) )

// Start = toString 10001

// Start = glue2 123000 456 // 123000456
//Start = glue2 765 432 // 765432



// 14

// . Given a positive integer, find the sum of the odd numbers up to that number starting from 1.
sumOdd :: Int -> Int
sumOdd n 
| n == 1 = 1
| n < 1 = abort "n has to be positive"
| n > 1 && isOdd n = n + sumOdd (n-1)
| n > 1 && isEven n = sumOdd (n-1)

/*
5 + sumOdd (4) = 5 + sumOdd 3 
5 + sumOdd 3 = 5 + 3 + sumOdd(2)
5 + 3 + sumOdd(2) = 5 + 3 + sumOdd(1)
5 + 3 + 1 = 9
*/


//Start = sumOdd 5 // 9 // 
//Start = sumOdd 21 // 121
//Start = sumOdd 10 // 25 = 9+7+5+3+1
//Start = sumOdd -13 // n has to be positive