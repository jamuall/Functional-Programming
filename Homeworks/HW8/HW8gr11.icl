module HW8gr11
import StdEnv


/*
Please write your NEPTUN code here:

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
We define a type RTriangle for right-angled triangles that are parallel to the
axes in a Cartesian coordinate system. We represent a triangle by its lower left
endpoint using the Point type.

Find the sum of the areas of all the triangles, whose lower left endpoint values
are nonnegative (>= 0).
*/

:: Point = {
		x :: Int,
		y :: Int
		}
		
:: RTriangle = {
		pos :: Point,
		width :: Int, 
		height :: Int
		}
		
r1 = { pos = {x = -5, y = 0}, width = 10, height = 10 }
r2 = { pos = {x = 0, y = -3}, width = 5, height = 10 }
r3 = { pos = {x = 0, y = 1}, width = 25, height = 24 }
r4 = { pos = {x = 10, y = 12}, width = 1, height = 1 }
r5 = { pos = {x = 12, y = 11}, width = 3, height = 24 }
r6 = { pos = {x = -32, y = 4}, width = 4, height = 24 }
r7 = { pos = {x = 0, y = 0}, width = 5, height = 12 }
r8 = { pos = {x = 4, y = -3}, width = 22, height = 21 }
r9 = { pos = {x = -100, y = -4}, width = 23, height = 22 }
		
//sum_areas :: [RTriangle] -> Int

//Start = sum_areas [r1, r2, r3] // 300
//Start = sum_areas [r1, r2, r3, r4, r5, r6, r7, r8, r9] // 366




//Task 2

/*
One suggestion to build a satisfactory password is to start with a memorable phrase
or sentence and make a password by extracting the first letter of each word.

Even better is to replace some of those letters with numbers:
    instead of including i or I put the number 1 in the password;
    instead of including o or O put the number 0 in the password;
    instead of including s or S put the number 5 in the password;

You can be sure that there are no punctiations, and each word (beside the last) is 
succeeded by a space.
*/

//generate :: String -> String

//Start = generate "a A i I o O s S x" // "aA110055x"
//Start = generate "Give me liberty or give me death" // "Gml0gmd"
//Start = generate "Keep Calm and Carry On" // "KCaC0"
//Start = generate "The only thing we have to fear Is fear itself" // "T0twhtf1f1"
//Start = generate "The way to get started is to quit talking and begin doing" // "Twtg51tqtabd"
//Start = generate "Life is trying things to See if they work" // "L1ttt51tw"
//Start = generate "s" // "5"
//Start = generate "" // ""

