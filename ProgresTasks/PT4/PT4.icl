module PT4
import StdEnv 


/*
Find the Maximum Even Element in a List of Lists:
Write a function that takes a list of lists, and your task is to find and return the
largest even number present in any of the sublists. If there are no even numbers in
the sublists, return -1.
Examples:
For the input [[1, 2, 3], [4, 5], [6, 7, 8]], the function should return 8, as it's the
largest even number in any of the sublists.
*/

maxEvenElement :: [[Int]] -> Int
maxEvenElement ls
| isEmpty evens = -1
= maxList evens
where
	evens = filter isEven(flatten ls) 
//Start = maxEvenElement [[1, 2, 3], [4, 5], [6, 7, 8]] // 8 
//Start = maxEvenElement [[1, 1, 3], [1, 1], [1, 7, 1], [1]] // -1
//Start = maxEvenElement [[1,1,1], []] // -1

