module HW2
import StdEnv

/*
Please write your NEPTUN code here: ZJUAR3

Your submission should not have any errors when running the code.
You'll receive a total of 100 points when you successfully solve both problems,
with 50 points awarded for each.

You should not delete anything from the given code, given test cases should stay
the same, but you can add your tests as well. Don't change the given function signatures. 
You can add your own functions, of course.

Make sure that you comment all 'Start'-s before submitting the code.
*/

//Task 1
/*
Find the sum of the numbers from a range (beginning to end) with step s.
The numbers are not guaranteed to be in the right order (the first number
may be greater than the second).
You can assume that the numbers are positive.
*/

seqSum :: Int Int Int -> Int
seqSum a b c 
| a < b = sum [a, (a+c)..b]
| a > b = sum [a, (a-c)..b]
 // 1 10 2 => [1, 3..10] => [1 3 5 7 9]

//Start = seqSum 1 5 1 // 1 + 2 + 3 + 4 + 5 = 15
//Start = seqSum 1 5 2 // 1 + 3 + 5 = 9
//Start = seqSum 1 5 3 // 1 + 4 = 5
//Start = seqSum 5 1 1 // 15



//Task 2
/*
Find the n-th index of the Padovan number sequence.
P(n) = P(n - 2) + P(n - 3) for n >= 3, with P(0) = P(1) = P(2) = 1.
Read more here: https://en.wikipedia.org/wiki/Padovan_sequence
*/

padovan :: Int -> Int
padovan n
| n < 0 = abort "invalid argument"
| n == 0 = 1
| n == 1 = 1
| n == 2 = 1
= padovan(n-2) + padovan(n-3)
//Start = padovan 4 // 2
//Start = padovan 6 // 4
//Start = padovan 9 // 9
//Start = padovan 12 // 21
//Start = padovan 0 // 1
//Start = padovan 1 // 1
//Start = padovan 2 // 1
//Start = padovan -4 // "invalid argument"