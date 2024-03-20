module PT5And3
import StdEnv

/*
You are given a tuple of lower and upper bounds and a list of tuples, where the first element is course
code and the second one is course score. Filter the list and return the list of course codes, which have
scores greater or equal to the lower bound and less or equal to the upper bound.
*/

rangeFilter :: (Int, Int) [(String,Int)] -> [String]
rangeFilter _ [] = []
rangeFilter (a,b) ls = [fst aa \\ aa<-ls | snd aa >= a && snd aa <= b ]

//Start = rangeFilter (10, 15) [("A",12),("B",3),("C",5),("E",14),("F",16)] // ["A","E"]
//Start = rangeFilter (3, 13) [("A",12),("B",3),("C",5),("E",14),("F",16)] // ["A", "B", "C"]
//Start = rangeFilter (5, 7) [] // []
//Start = rangeFilter (15, 3) [("A",12),("B",3),("C",5),("E",14),("F",16)] // []
//Start= rangeFilter (0, 2) [("A",12),("B",3),("C",5),("E",14),("F",16)] // []


/*
2. Given a list of lists of Int.
For each element greater than 5 of each sublist create a triple like (number, sum, product)
where
number -> the number itself
sum -> the sum of all the integers in the [1,number) interval
product -> product of all the integers in the [1,number)

Example:
[[1,2,4],[6,8,2,1]] -> [[],[(6,15,120),(8,28,5040)]]
because 1,2,4,2,1 are not greater than 5,
15 = 1+2+3+4+5, 120 = 1*2*3*4*5
28 = 1+2+3+4+5+6+7. 5040 = 1*2*3*4*5*6*7
*/

numSumProd::[[Int]]->[[(Int,Int,Int)]]
numSumProd [] = []
numSumProd ls = [[(a, sum[1,2..a], prod [1,2..a])\\ a<-a]\\ a<-mainLs] 
where mainLs = [(filter (\x = x>5) a)\\a<-ls ]
//Start = numSumProd [[1,2,4],[6,8,2,1]]//[[],[(6,21,720),(8,36,40320)]] // it should be the number included
//Start = numSumProd [[1..6],[1..5]]//[[(6,15,120)],[]] // it should be the number not included
//Start = numSumProd [[1,2,2],[]]//[[],[]]
//Start = numSumProd []//[]





