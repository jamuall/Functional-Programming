module HW5gr11
import StdEnv

//Task 1
/*
	You are given a list of lists of integers. You need to find the even elements
	that are common in all the lists. 
	
	For the sake of simplicity, sort the output list.
*/
ifOneEmpty :: [[Int]] -> Bool
ifOneEmpty [] = False
ifOneEmpty [x:xs] 
| x == [] = True
= ifOneEmpty xs
//Start = ifOneEmpty [[]]

commAux :: Int [[Int]] -> Bool
commAux x y 
| y == [] = True
= isMember x (hd y) && commAux x (tl y)
//Start = commAux 2 [[1,2,3],[5,3,2],[7,3,2]]


comm_elem :: [[Int]] -> [Int]
comm_elem x
| y == [] || ifOneEmpty y = []
= sort[a \\ a <- hd y | commAux a y]
where y = map(filter isEven) x

//Start = comm_elem [[1,2,3],[5,3,2],[7,3,2]] // [2]
//Start = comm_elem [[0,1,4],[0,1,4],[0,1,4]] // [0,4]
//Start = comm_elem [[1,2,3,4],[5,6,7,8],[9,10,11,12]] // []
//Start = comm_elem [[1,2,3],[6,7,8],[6,7,8]] // []
//Start = comm_elem [[1,5,3,7,8,6],[2,4,6,3,8,1],[13,54,22,1,2,6],[6,1,2]] // [6]
//Start = comm_elem [[2,10,3,4,6],[4,6,2,10,2,3],[6,4,2,3,5,10]] // [2,4,6,10]
//Start = comm_elem [[1,2,3,4]] // [2,4]
//sStart = comm_elem [] // []

//Task 2
/*
	Between 2 parallel walls there are a number of obstacles of different heights, but
	with the same distance between them. Your task is to find 2 obstacles for which
	the water volume is the greatest.
	
	Example: largest_volume [5, 2, 1, 1, 4, 2] == (0, 4)
			 
			 We can imagine the list in the following way:
			 
			 |		             |  
			 |		 |           |x	x x x|  
			 |		 |     ==>   |x x x x|
			 | |     | |         |x|x x x| |
			 | | | | | |         |x|x|x|x| |
			 -----------         -----------
			 0 1 2 3 4 5         0 1 2 3 4 5
	                            (16 sq units)
	                             
	         We can tell that the obstacles 0 and 4 will produce the greatest
	         volume. 
	
	The output should contain the indexes of the 2 columns.
	You do not need to check the input, consider that you have at least 2 obstacles.
	Also, do not consider the width of the obstacles.
*/

//largest_volume :: [Int] -> (Int, Int)
	
//Start = largest_volume [5, 2, 1, 1, 4, 2] // (0,4)
//Start = largest_volume [1,3,4,10,5,2,10] // (3,6)
//Start = largest_volume [1,1,5,5,1] // (2,3)
//Start = largest_volume [1,1,5,5,1,1,1,1,1] // (0,8)
//Start = largest_volume [1,1] // (0,1)
//Start = largest_volume [1,2,3,4,5,5,4,3,2,1] // (2,7)
//Start = largest_volume [1,2,3,4,4,3,2,1] // (1,6)
//Start = largest_volume [1,2,2,3,3,5] // (1,5)
//Start = largest_volume [1,2,3,3,10,10] // (4,5)