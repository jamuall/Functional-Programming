module arrays
import StdEnv

//-------------1------------

arrayToList :: {Int} -> [Int]
arrayToList arr = [x \\ x <-: arr]

listToArray :: [Int] -> {Int}
listToArray list = {c \\ c <- list}

charsToStr :: [Char] -> String
charsToStr charList = {x \\ x <- charList}

//Start = charsToStr ['c','l','e','a','n']

//-------------2------------
/* Given a character and a String, remove the given character from the string */

remChar :: Char String -> String

remChar ch str = toString (filter (\x = (x <> ch)) arr)
where arr = [c \\ c <-: str]
//Start = remChar 'c' "hello" // "hello"
//Start = remChar 'a' "apple" // "pple"
//Start = remChar 'm' "midterm" // "idter"

//-------------3------------
// Given an array of lists of integers and an integer, 
// keep the lists whose difference between max and min 
// element squared is greater than the given number
// There are no [] in the array.
	
minMaxDiff::{[Int]} Int->{[Int]}
minMaxDiff arr n = {x \\ x <- rr}
where 
	ls = [x \\ x <-: arr]
	rr = [a \\ a<-ls | (maxList a - minList a)^2 > n ]
	
//Start = minMaxDiff {[1,21,2],[1,1,1,1,1],[1]} 5//{[1,21,2]}
//Start = minMaxDiff {[1,21],[1..10],[4,3]} 5//{[1,21],[1,2,3,4,5,6,7,8,9,10]}
//Start = minMaxDiff {[1..10],[5..6]} -3//{[1,2,3,4,5,6,7,8,9,10],[5,6]}


//-------------4------------
// Given array find max of it and return new array which has 
// all occurrences of maximum removed.
// E.g. {1,4,5,3,3,2,4,5,1,3,4} max is 5 -> {1,4,3,3,2,4,1,3,4}.

rem_max :: {Int} -> {Int}
rem_max arr = { b \\ b<-aa}
where 
	ls = [a\\ a <-: arr]
	max = maxList ls
	aa =[ a\\ a<-ls | a<>max]
		

//Start = rem_max {1,4,5,3,3,2,4,5,1,3,4} //{1,4,3,3,2,4,1,3,4}
//Start = rem_max {1,42,42,52,452,4} // {1,42,42,52,4}
//Start = rem_max {5} // {}
//Start = rem_max {} // {}


//-------------5------------
// Given two arrays, return new array such that i-th element of it is 
// maximum of i-th element of first and second arrays.
// E.g. when we calculate 5th element of result array, we look at 
// 5th element of first and 5th element of second arrays, and choose maximum of the two.
// You can assume that arrays have same length. 
maxTwo :: Int Int -> Int
maxTwo a b
| a > b = a
= b

maxOfTwo :: {Int} {Int} -> {Int}
maxOfTwo arr1 arr2 = listToArray newls
where 
	ls1 = arrayToList arr1
	ls2 = arrayToList arr2
	newls = [(maxTwo a b) \\ a<-ls1 & b <-ls2 ]

//Start = maxOfTwo {} {} // {}
//Start = maxOfTwo {1} {5} // {5}
//Start = maxOfTwo {1,5,4} {2,3,6} // {2,5,6}
//Start = maxOfTwo {1,2,3,4,5} {1,2,3,4,5} // {1,2,3,4,5}


//-------------6------------
// You are given array of integers.
// Your function should return true if each value appears at least twice 
// in the array, and it should return false
// if any element is distinct.
occurence :: Int [Int] -> Int
occurence a [] = 0
occurence a ls
| a == hd ls = 1 + occurence a  (tl ls)
= occurence a  (tl ls)
//Start = occurence 2 [1,2,2,5]
ifAllTrue :: [Bool] -> Bool
ifAllTrue [] = True
ifAllTrue [x:xs] = x && ifAllTrue xs 

f :: {Int} -> Bool
f arr = ifAllTrue [(occurence a ls >= 2) \\ a <- lsds]
where
	ls = arrayToList arr
	lsds = removeDup ls

//Start = f {1,2,3,1,3,2,2,2} // True
//Start = f {1,2,3,4,3,2,1} // False
//Start = f {1,1,1,3,3,4,3,2,4,2} // True



//-------------7------------
// An array is monotonic if it is either monotone increasing or monotone decreasing
// A is monotone increasing if for all i<=j, A[i]<=A[j]
// A is monotone decreasing if for all i<=j, A[i]>=A[j]
// Given array, your task is to decide if it is monotonic.
isInc :: [Int] -> Bool
isInc [] = True
isInc [x] = True
isInc x
| (hd x <= hd (tl x)) = isInc (tl x)
= False
//Start = isInc [3,4,4,5,6]

isDec :: [Int] -> Bool
isDec [] = True
isDec [x] = True
isDec x
| (hd x >= hd (tl x)) = isDec (tl x)
= False
//Start = isDec [6,5,4,4]


isMonotonic :: {Int} -> Bool
isMonotonic arr = (isInc ls) || (isDec ls)
where ls = arrayToList arr

//Start = isMonotonic {6,5,4,4} // True
//Start = isMonotonic {1,3,2} // False
//Start = isMonotonic {1,2,4,5} // True
//Start = isMonotonic {1,1,1} // True


//-------------8------------
/* Given an integer array and the integer list, which is an index list. 
Return the list of integers, which are the values at the indecses 
of the given array. Indexing starts at 0.
E.g: {23,45,346,6,7,73,92} [0,2,3,4,6] => [23,346,6,7,92]
If the given index is invalid, then no need to include it.
{23,45,346,6,7,73,92} [6,-10,1,0,8] => [92,45,23]
Index -10 and 8 are ignored because they are out of bound. */

subList :: {Int} [Int] -> [Int]
subList arr ls = [arr.[a] \\ a<-ls | a < n && a >= 0]
where n = length(arrayToList arr)
Start = subList {23,45,346,6,7,73,92} [0,2,3,4,5] // [23,346,6,7,73]
//Start = subList {-23,0,90,76,354,7} [1,0,-5,8] // [0,-23]
//Start = subList {} [3,5,-6,2] // []
//Start = subList {4,3,6,7,8} [-1,-2] // []





