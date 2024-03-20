module HW6gr11
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
	You are given a range: a start and an end. You need to find all the possible
	pythagorean integers (numbers that match the pythagorean identity: a^2 + b^2 = c^2)
	within the given range. (a, b, c: are in the given range).
	
	You need to return the number of such triplets.
	
	Example: all_pyth 1 5 -> 2
	
			 because the only triplets are [(3,4,5),(4,3,5)]
			 3^2 + 4^2 = 5^2 (9 + 16 = 25)
			 4^2 + 3^2 = 5^2 (16 + 9 = 25)
*/

all_pyth :: Int Int -> Int
all_pyth a b = length[(x,y)\\ x<-[a,a+1..b] , y<-[a,a+1..b] | isMember (x^2 + y^2) sqls]
where sqls = [aa * aa \\ aa<- [a,a+1..b]]
//Start = all_pyth 1 5 // 2
//Start = all_pyth 1 13 // 6
//Start = all_pyth 1 25 // 16
//Start = all_pyth 50 100 // 12
//Start = all_pyth 100 300 // 126
//Start = all_pyth 35 87 // 22



//Task 2

/*
	It's the end of trick-or-treating and we have a list representing how much
	candy each child in our group has made out with. We don't want the kids to start
	arguing.

	At the same time, we want to reward the ones that recieved a lot of candies. So 
	we want to match the amount of candies each kid recieves to the nth minimum.
	
	You are given the list and the number of the minimum. You need to return the number
	of candies needed to complete the task
	
	Example: candies [1,6,2,4] 3 -> 5
			 
			 firstly, we find the third minimum:
			 	1st minimum is 1
			 	2nd minimum is 2
			 	3rd minimum is 4
			 	
			 then we count how many candies we need so that every kid has at least 4 candies:
			 	the first one needs 3
			 	the second does not need extra
			 	the third needs 2
			 	the fourth does not need extra
			 	
			 so the total is 3+2 = 5
*/

minLs :: [Int] Int -> Int
minLs ls x
| x == 1 = minList ls
= minLs (removeMember(minList ls) ls) (x-1)
//Start = minLs [1,6,2,4] 3 
candies :: [Int] Int -> Int
candies ls x =  sum[c-a\\a<-mainls]
where
	c = minLs ls x
	mainls = filter (\i = i <= c) ls
	
//Start = candies [1,6,2,4] 3 // 5
//Start = candies [1,6,2,4] 4 // 11
//Start = candies [5,8,6,4] 4 // 9
//Start = candies [1,1,1,2] 2 // 0
//Start = candies [10,5,10,10,10,6] 3 // 9
//Start = candies [12,53,23,11,54,23] 3 // 23