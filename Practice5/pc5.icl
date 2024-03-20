module pc5
import StdEnv

occurence :: Int [Int] -> Int
occurence n list
| list == [] = 0
| n == hd list = 1 + occurence n (tl list)
= occurence n (tl list)
//Start = occurence 5 [1, 2, 3, 5, 5, 5] // 3

removeNum :: Int [Int] -> [Int]
removeNum x list
| list == [] || x == 0 = []
| hd list == x = removeNum x (tl list)
= [hd list] ++ removeNum x (tl list)

//Start = removeNum 5 [5, 1, 2, 3, 5, 5, 5, 4] // [1, 2, 3, 4]

removeDuplicates :: [Int] -> [Int]
removeDuplicates [] = []
removeDuplicates [x:xs]
| occurence x [x:xs] > 1 = removeDuplicates(removeNum x [x:xs])
= [x : removeDuplicates xs]

//Start = removeDuplicates [1,2,3,4,1,1,4,1,7,8] // [2,3,7,8]

count2 :: Int [Int] -> Int
count2 x ls = length (filter (\n = n == x) ls) 
Start = count2 5 [5,5,5,5,5,6]

count :: Int [Int] -> Int
count x xs = length [1 \\ y <- xs | y == x]

remDup :: [Int] -> [Int]
remDup xs = [x \\ x <- xs | count x xs == 1]

//Start = remDup [1,2,3,4,1,1,4,1,7,8]





