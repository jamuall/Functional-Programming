module HW3
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
An element in a list is dominant if it is greater than all elements to its
right. You will be given a list and your task will be to return a list of
all dominant elements. For example:

[1,21,4,7,5] -> [21,7,5] because 21, 7 and 5 are greater than elments 
						 to their right. 
[5,4,3,2,1] -> [5,4,3,2,1]

Notice that the last element is always included. All numbers will
be greater than 0.
*/

domNumsAux :: Int [Int] -> Bool
domNumsAux x list
| list == [] = True
| x > (hd list) = domNumsAux x (tl list)
= False
//Start = domNumsAux 5 [4, 3, 2, 1]

dom_nums :: [Int] -> [Int]
dom_nums [] = []
dom_nums x
| domNumsAux (hd x) (tl x) == True = [ hd x : dom_nums (tl x)]
= dom_nums (tl x)


//Start = dom_nums [1,21,4,7,5] // [21,7,5]
//Start = dom_nums [16,17,14,3,14,5,2] // [17,14,5,2]
//Start = dom_nums [92,52,93,31,89,87,77,105] // [105]
//Start = dom_nums [75,47,42,56,13,55] // [75,56,55]
//Start = dom_nums [67,54,27,85,66,88,31,24,49] // [88,49]
//Start = dom_nums [76,17,25,36,29] //[76,36,29]
//Start = dom_nums [104,18,37,9,36,47,28] // [104,47,28]


//Task 2
/*
The town sheriff dislikes even numbers and wants all even numbered families
out of town! In town crowds can form and individuals are often mixed with
other people and families. However you can distinguish the family they belong
to by the number on the shirts they wear. As the sheriff's assistant it's
your job to find all the even numbered families and remove them from the town!

You are given a list of numbers. The numbers each repeat a certain number of
times. Remove all numbers that repeat an even number of times while keeping
everything else the same.

[1, 2, 3, 1, 3, 2, 3] -> [3, 3, 3]
    the number 1 appears twice
    the number 2 appears twice
    the number 3 appears three times

	1 and 2 both appear an even number of times, so they are removed
	from the list. The final result is: [3, 3, 3]
*/

evenAux :: Int [Int] -> Int
evenAux n list
| list == [] = 0
| n == hd list = 1 + evenAux n (tl list)
= evenAux n (tl list)

//Start = evenAux 5 [1, 2, 3, 5, 5, 5] //3

even_out :: [Int] -> [Int]
even_out xs = [x \\ x <- xs | isOdd(evenAux x xs)]


/*
removeNum :: Int [Int] -> [Int]
removeNum x list
| list == [] || x == 0 = []
| hd list == x = removeNum x (tl list)
= [hd list] ++ removeNum x (tl list)

//Start = removeNum 5 [5, 1, 2, 3, 5, 5, 5, 4] // [1, 2, 3, 4]


even_out :: [Int] -> [Int]
even_out list
| length list == 0 = []
| isEven(evenAux (hd list) list) = even_out (removeNum (hd list) list)
= repeatn (evenAux (hd list) list) (hd list) ++ even_out (tl list)
*/

//Start = even_out [1, 2, 3, 1, 3, 2, 3] // [3, 3, 3]
//Start = even_out [26, 23, 24, 17, 23, 24, 23, 26] // [23,17,23,23]
//Start = even_out [1, 2, 3] // [1,2,3]
//Start = even_out [1, 2, 2] // [1]
//Start = even_out [75, 68, 75, 47, 68] // [47]
//Start = even_out [42, 94, 32, 42, 4, 94, 4, 67, 4, 67, 32] // [4, 4, 4]
//Start = even_out [13, 14, 13, 15, 14, 22, 22, 12, 14] // [14,15,14,12,14]
//Start = even_out [82, 58, 71, 58, 44, 79, 50, 44, 79, 67, 82, 82, 55, 50] // [82,71,67,82,82,55]