module solve1
import StdEnv

// 1. Write a function to compute the absolute value of an integer number.

//Start = abs -4
myabs :: Int -> Int
myabs x
| x < 0 = ~x
| x == 0 = x
| x > 0 = x

//Start = myabs -4

// 2. Define a function maxi with two arguments that delivers the maximum of the two.
maxi :: Int Int -> Int
maxi x y
| x > y = x
= y

// First, function takes 2 variables, so we should declare maxi x y, then with if statement | we write first condition, then else =y


//Start = maxi 4 5


// 3. Add 100 to a number.
add100 :: Int -> Int
add100 x = x + 100
//Start = add100 50

// 4. Triple a number.
triple :: Int -> Int
triple x = x*3
//Start = triple 5

// 5. Check if a number is odd.
isoddnr :: Int -> Bool  // isEven, isOdd built-in function

isoddnr x = ((x rem 2) == 1)
// there is no % in clean, you should write rem
// when there is a function int to bool, isodd x = ((x rem 2) == 1)

//Start = isoddnr 5

// 6. Say Hello to the World!
st :: String String String -> String
st x y z = x +++ y +++ z

//Start = st "Hello " "World " "Jamal"  


// 7. Compute the cube of a number.
cube :: Int -> Int

cube x = x ^ 3

//Start = cube 5

// 8. Define a function mini that has two arguments that delivers the minimum of the two.
mini :: Int Int -> Int
mini x y 
| x < y = x
=y

//Start = mini 5 8

// 9. Find the 10th muliple of a number.
times10 :: Int -> Int

times10 x = x*10

//Start = times10 5

// 10. Check if a number is even.
isevenr :: Int -> Bool  // isEven, isOdd built-in function

isevenr x = ((x rem 2) == 0)

//Start = isevenr 5

// 11. Given an integer, write a function that returns the last digit.

lastDigit :: Int -> Int

lastDigit x = (x rem 10)

//Start = lastDigit 125

// 12. Check if a number is multiple of 10.
ismult10 :: Int -> Bool

ismult10 x  = ((x rem 10) == 0)

//Start = ismult10 100

// 13. Change a character if is small letter to uppercase and vice versa,
// if is a special sign, leave as it is.
ch :: Char -> Char
ch c

| 65 <= (toInt c) && (toInt c) <= 90 = toLower c
| 97 <= (toInt c) && (toInt c) <= 122 = toUpper c
= c

//Start = (ch 'a', ch 'z', ch 'A', ch 'Z', ch 'x', ch '%')

ch2 :: Char -> Char
ch2 c
| isUpper c = toLower c
| isLower c = toUpper c
= c

// 14. Add the numbers from 1..N in a recursive function, where N is positive.
addn :: Int -> Int
addn n // 1 + 2 + 3 + 4 + 5
| n <= 0 = abort " N can not be zero or negative" //for giving text output use abort
| n == 1 = 1
= n + addn(n-1)

//Start = addn 5

// 15. Compute factorial n recursively, where n! = n*(n-1)! .
factor :: Int -> Int
factor n
| n < 0 = abort "N cannot be negative"
| n == 0 = 1
| n == 1 = 1
= n * factor(n-1)

//Start = factor 5

// 16. Count the number of digits of a number.
countDigits :: Int -> Int
countDigits x 
| x < 10 = 1
= 1 + (countDigits (x/10))

//Start = countDigits 500

// 17. Add the digits of a number e.g. for 123 is 6.
sumDigits :: Int -> Int
sumDigits x 
| x < 10 = x 
= (x rem 10) + sumDigits (x/10) 

//Start = sumDigits 68

// 18. Write a function that takes two arguments, say n and x, and computes their power,
// in 2 versions - with recursion and without recursion.
power :: Int Int -> Int

power x y
| y == 0 = 1
= x * (power x (y-1)) 

//Start = power 2 5

// 19. Given three integer numbers a, b and c. 
// Check if both a and b have the same remainder when divided by c.
sameRem :: Int Int Int -> Bool

sameRem a b c  = (a rem c) == (b rem c) 

//Start = sameRem 32 42 5


// 20. Given two integers and a boolean value. 
// Check if the first integer is even, the second divisible by 13 
// and the boolean value is True

check :: Int Int Bool -> Bool
check a b boolean = (a rem 2 == 0) && (b rem 13 == 0) && boolean

//Start = check 4 26 (isEven 24)

// 21. Check if a number is the sum of two other given numbers in any order.
// (a == b + c) any order
issum :: Int Int Int -> Bool

issum a b c = (a == b + c) || (b == a + c) || (c == a + b)

//Start = issum 10 15 25

// 22. Check if a number is divisible by 9! (using sumDigits)
// a number is divisible by 9 is the sum of digits is divisible by 9
div9 :: Int -> Bool

div9 x
|(sumDigits x) rem 9 == 0 = True
|otherwise = False

//Start = div9 23131233

// 23. Use 18. to write a function that squares its argument.
sq :: Int -> Int
sq x = power x 2

//Start = sq 5

// 24. Count how many ending 0 digits are in a number e.g. x = 50000 result is 4
count :: Int -> Int
count x 
| x rem 10 == 0 = 1 + count (x/10)
= 0

Start = count 5500


