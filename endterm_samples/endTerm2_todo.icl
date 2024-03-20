module endTerm2_todo

import StdEnv 


/*
1. Given two integer numbers k and n.
Generate a String by the following pattern:
In case k=2 and n=3
"[[**[[**[[**"
So n times, k opening brackets followed by k stars.
*/

bracketsStars::Int Int -> String
bracketsStars k n = {m\\m<-main}
where 
	ls = ['['\\a<-[1,2..k]]
	lst = ['*'\\a<-[1,2..k]]
	main = flatten[ls ++ lst\\a<-[1,2..n]]
//Start = bracketsStars 2 3 // "[[**[[**[[**"
//Start = bracketsStars 0 3 // ""
//Start = bracketsStars 5 2 // "[[[[[*****[[[[[*****"


/*
2. Write a function which takes a String and modifies it such that
only the first character is uppercase and every other one is lowercase.
The given string contains only letters!

Hints:
toInt('a') - toInt('A') == 32
Uppercase letters are in the range [65,90] (ASCII)
Lowercase letters are in the range [97,122] (ASCII)

Example:
"abcdFeH" = "Abcdfeh"
*/

capitalize::String -> String
capitalize "" = ""
capitalize str = {a\\a<-newls}

where 
	ls = [s\\s<-:str]
	newls = [toUpper(hd ls)] ++ [toLower a\\a<-(tl ls)]
//Start = capitalize "aABCDEFG" // "Aabcdefg"
//Start = capitalize "abABab" // "Ababab"
//Start = capitalize "" // ""


:: Major = Finance | CS | Math | Physics | Economy | Linguistics
:: Course = {name::String, major::Major, credits::Int}

OOP::Course
OOP = {name="OOP",major=CS, credits=5}
Discrete_math::Course
Discrete_math = {name="Discrete_math",major=Math, credits=4}
Relativity::Course
Relativity = {name="Relativity", major=Physics, credits=6}
Functional::Course
Functional = {name="Functional", major=CS, credits=5}
Quantum_mechanics::Course
Quantum_mechanics = {name="Quantum_mechanics", major=Physics, credits=4}
Corporate_finance::Course
Corporate_finance = {name="Corporate_finance", major=Finance, credits=6}
Venture_captical::Course
Venture_captical = {name="Venture_captical", major=Finance, credits=6}
Macroeconomics::Course
Macroeconomics = {name="Macroeconomics", major=Economy, credits=6}
Microeconomics::Course
Microeconomics = {name="Microeconomics", major=Economy, credits=6}
Numerical_Methods::Course
Numerical_Methods = {name="Numerical_Methods", major=Math, credits=4}
Cryptography::Course
Cryptography = {name="Cryptography", major=CS, credits=2}
Phonology::Course
Phonology = {name="Phonology", major=Linguistics, credits=3}
Morphology::Course
Morphology = {name="Morphology", major=Linguistics, credits=3}


/*
3. Given a list of Courses and a major, check if any of the courses in the list
has the same major with the given one, return True if there is at least one.
*/

instance == Major 
where
	(==) Finance Finance = True
	(==) CS CS = True
	(==) Math Math = True
	(==) Physics Physics = True
	(==) Economy Economy = True
	(==) Linguistics Linguistics = True
	(==) _ _ = False
same_major :: [Course] Major -> Bool
same_major ls mj = or[a.major == mj\\a<-ls]
//Start = same_major [Corporate_finance, OOP, Microeconomics] Finance // True
//Start = same_major [Morphology, Macroeconomics, Quantum_mechanics] CS // False
//Start = same_major [Venture_captical, Relativity, Cryptography] Physics // True
//Start = same_major [Discrete_math] Economy // False
//sStart = same_major [] Physics // False


/*
4. Given a list of Courses that a student has taken, write a function that
returns a list of records (you need to define by yourself, called Earned_credits),
each records has the name of the major (field) and
the credits the student earned for each major (earned).
*/

:: Earned_credits = {major1 :: Major, credits1 :: Int}

addCredits :: Earned_credits [Course] -> Earned_credits
addCredits {major1=m, credits1=c} courses = {major1=m, credits1= sum[c.credits\\c<-courses | c.major == m]+c }

calc :: [Course] -> [Earned_credits]
calc [] = []
calc coursels = map (\m -> addCredits {major1=m, credits1=0}coursels) majorls
where
	majorls = removeDup[c.major\\c<-coursels]
	
//Start = calc [Functional, OOP, Relativity] // [(Earned_credits CS 10),(Earned_credits Physics 6)]
//Start = calc [Morphology, Macroeconomics, Numerical_Methods] // [(Earned_credits Math 4),(Earned_credits Economy 6),(Earned_credits Linguistics 3)]
//Start = calc [Corporate_finance, Numerical_Methods, Cryptography] // [(Earned_credits Finance 6),(Earned_credits Math 4),(Earned_credits CS 2)]
//Start = calc [] // []


/*
5. Write '+' operator for lists.
If both lists are sorted in increasing order you should merge them
in a way that resulting list is sorted too.
Ex.: [1,3,6] + [2,4,5,7] -> [1,2,3,4,5,6,7]
If list is not sorted than it is considered empty.
Ex.: [1,3,6] + [2,3,1] -> [1,3,6] + [] -> [1,3,6]
Ex.: [2,9,7] + [5,4,3] -> [] + [] -> []
*/
isSorted :: [a] -> Bool | Eq, Ord a
isSorted ls = sort ls == ls

instance + [a] | Eq, Ord a
where 
	(+) ls1 ls2
	|isSorted ls1 && isSorted ls2 = sort(ls1 ++ ls2)
	|isSorted ls1 && not(isSorted ls2) = ls1
	|not(isSorted ls1) && isSorted ls2 = ls2
	|not(isSorted ls1) && not(isSorted ls2) = []
//Start = [1,2,3] + [1,3,6] // [1,1,2,3,3,6]
//Start = [1,3,6] + [2,4,5,7] // [1,2,3,4,5,6,7]
//Start = [1,3,6] + [2,3,1] // [1,3,6]
//Start = [5,1] + [1,3,6] // [1,3,6]
//Start = [] + [1] // [1]
//Start = [2,3,1] + [12,3,1] // []
//Start :: [Int]
//Start = [] + [] // []


:: Tree a = Node a (Tree a) (Tree a) | Leaf

tree1 :: Tree Int
tree1 = (Node 4 (Node 10 (Node 6 Leaf Leaf)(Node 11 Leaf Leaf)) (Node 20 (Node 12 Leaf Leaf) Leaf))
tree2 :: Tree Int
tree2 = (Node 5 (Node 10 (Node 31 (Node 1 Leaf Leaf) Leaf) Leaf) (Node 17 (Node 31 (Node 14 (Node 12 Leaf Leaf) Leaf) Leaf) (Node 11 Leaf Leaf) ))
tree3 :: Tree Int
tree3 = (Node 12 (Node 11 (Node 11 (Node 32 Leaf Leaf) Leaf) Leaf) (Node 4 (Node 17 (Node 5 (Node 7 Leaf Leaf) Leaf) Leaf) (Node 3 Leaf (Node 4 Leaf Leaf)) ))
tree4 :: Tree Int
tree4 = (Node 7 (Node 11 tree1 tree2) (Node 5 tree3 tree2))
tree5 :: Tree Int
tree5 = Node 1 tree3 tree4


/*
6. Define == for Tree. Two Trees are equal if each node value
from 1st tree is equal to 2nd tree.
*/
isEqual :: (Tree Int) (Tree Int) -> Bool
isEqual Leaf Leaf = True
isEqual (Node a l r) (Node b ll rr)
| a == b = (isEqual l ll) && (isEqual r rr)
= False
//Start = isEqual tree1 tree1
instance == (Tree a) | == a
where
	(==) Leaf Leaf = True
	(==) (Node a l r) (Node b ll rr)
	| a == b = (l == ll) && (r == rr)
	= False
//Start = tree1 == tree1 // True
//Start = tree2 == tree3 // False
//Start = tree4 == tree4 // True
//Start = tree1 == tree5 // False


:: Student = {s_name::String, grades::[Int]}

A :: Student
A = {s_name="A", grades = [1,2,3]}
B :: Student
B = {s_name="B", grades = [1,2,3,4]}
C :: Student
C = {s_name="C", grades = [1,2,3,4,5]}
D :: Student
D = {s_name="D", grades = [1,2,3,4,5,6]}
E :: Student
E = {s_name="E", grades = [1]}
F :: Student
F = {s_name="F", grades = [1,2]}


/*
7. Having a list of students, each student has a list of grades
create a balanced (or almost balanced) binary search tree based on the students' grades average.
Info: balance tree means that the difference between the depth of the left tree and the depth of 
the right tree is less than 1
Note: There is no dublication in the averages.
*/
listtoTree :: [Student] -> Tree Student 
listtoTree [] = Leaf
listtoTree students = Node (students !! middle) (listtoTree fst) (listtoTree snd)
where
	middle = (length students) / 2
	fst = take middle students
	snd = drop (middle + 1) students

lss = [1..10]
//Start = listtoTree lss

createStudTree :: [Student] -> (Tree Student)
createStudTree [] = Leaf
createStudTree stdls = listtoTree lsls
where
	lsls= sortBy (\s1 s2 = avg s1.grades < avg s2.grades) stdls
//Start = createStudTree [] // Leaf
//Start = createStudTree [A,B,C] //(Node (Student "B" [1,2,3,4]) (Node (Student "A" [1,2,3]) Leaf Leaf) (Node (Student "C" [1,2,3,4,5]) Leaf Leaf))
//Start = createStudTree [A,B,C,D] //(Node (Student "C" [1,2,3,4,5]) (Node (Student "B" [1,2,3,4]) (Node (Student "A" [1,2,3]) Leaf Leaf) Leaf) (Node (Student "D" [1,2,3,4,5,6]) Leaf Leaf))
//Start = createStudTree [C,D,A,B] //(Node (Student "C" [1,2,3,4,5]) (Node (Student "B" [1,2,3,4]) (Node (Student "A" [1,2,3]) Leaf Leaf) Leaf) (Node (Student "D" [1,2,3,4,5,6]) Leaf Leaf))
//Start = createStudTree [F,E,C,D,A,B] //(Node (Student "B" [1,2,3,4]) (Node (Student "F" [1,2]) (Node (Student "E" [1]) Leaf Leaf) (Node (Student "A" [1,2,3]) Leaf Leaf))
// (Node (Student "D" [1,2,3,4,5,6]) (Node (Student "C" [1,2,3,4,5]) Leaf Leaf) Leaf))

/*
8.Given binary search tree and Integer value, remove all the nodes from the tree which have this value.
Resulting tree should maintain binary search tree property.
*/
makebst :: [Int] -> (Tree Int)
makebst [] = Leaf
makebst ls = Node (ls !! middle) (makebst fst) (makebst snd)

where 
	middle = length ls / 2
	fst = take middle ls 
	snd = drop (middle + 1) ls

treels :: (Tree Int) -> [Int]
treels Leaf = []
treels (Node a l r) = treels l ++ [a] ++ treels r


removeInt :: Int (Tree Int) -> (Tree Int)
removeInt _ Leaf = Leaf
removeInt num (Node a l r)
| num < a = Node a (removeInt num l) r
| num > a = Node a l (removeInt num r)
= removeInt num (mergeTrees l r)

// Merges two trees while maintaining BST properties
mergeTrees :: (Tree Int) (Tree Int) -> (Tree Int)
mergeTrees Leaf r = r
mergeTrees l Leaf = l
mergeTrees l r = Node minRight l newRight
where
	(minRight, newRight) = deleteMin r

// Deletes the minimum node from the tree and returns the value and new tree
deleteMin :: (Tree Int) -> (Int, Tree Int)
deleteMin (Node a Leaf r) = (a, r)
deleteMin (Node a l r) = (minLeft, Node a newLeft r)
where
	(minLeft, newLeft) = deleteMin l
//Start = removeInt 5 (Node 4 (Node 3 (Node 3 (Node 2 (Node 1 Leaf Leaf) Leaf) Leaf) (Node 4 Leaf Leaf)) (Node 5 (Node 5 Leaf Leaf) (Node 6 Leaf Leaf))) // (Node 4 (Node 3 (Node 3 (Node 2 (Node 1 Leaf Leaf) Leaf) Leaf) (Node 4 Leaf Leaf)) Leaf)
//Start = removeInt 1 (Node 1 (Node 1 (Node 1 (Node 1 (Node 1 (Node 1 Leaf Leaf) Leaf) Leaf) Leaf) Leaf) Leaf) // Leaf


/*
9.Complementary colors are pairs of colors which, when combined or mixed,
cancel each other out (lose hue) by producing a grayscale color like white or black.
Such pairs are:
Red - Green
Orange - Blue
Yellow - Purple
Violet - Amber
Teal - Vermilion
Magenta - Chartreuse
Create an instance == for the Color and
write a function that finds complement of a given color.
*/

:: Color = Red | Yellow | Green | Blue | Purple | Orange | Violet | Amber | Teal | Vermilion | Magenta | Chartreuse

colorList = [Red, Yellow, Green, Blue, Purple, Orange, Violet, Amber, Teal, Vermilion, Magenta, Chartreuse]

instance == Color 
where
    (==) Red Red = True
    (==) Yellow Yellow = True
    (==) Green Green = True
    (==) Blue Blue = True
    (==) Purple Purple = True
    (==) Orange Orange = True
    (==) Violet Violet = True
    (==) Amber Amber = True
    (==) Teal Teal = True
    (==) Vermilion Vermilion = True
    (==) Magenta Magenta = True
    (==) Chartreuse Chartreuse = True
    (==) _ _ = False

find_complement :: Color -> Color
find_complement color = case color of
    Red -> Green
    Green -> Red
    Orange -> Blue
    Blue -> Orange
    Yellow -> Purple
    Purple -> Yellow
    Violet -> Amber
    Amber -> Violet
    Teal -> Vermilion
    Vermilion -> Teal
    Magenta -> Chartreuse
    Chartreuse -> Magenta

//Start = find_complement Red // Green
//Start = find_complement Green // Red
//Start = find_complement Teal // Vermilion
//Start = find_complement Chartreuse // Magenta
//Start = find_complement Violet // Amber


/*
10.Create a class called Comparisons and define the binary operations:
*== , != , *< , *> , *<= ,*>=
Given two elements it compares them and gives out a boolean.
Example: 
x *== y should check if the x and y are equal.
!= -> not equal
*< -> less (smaller)
*> -> greater (bigger)
*<= -> less (smaller) or equal
*>= -> greater (bigger) or equal
Make an instance for Vector3 type. 
instance Comparisons Vector3
To compare two vectors use their length.
Length of 3 dimensional vector (a,b,c) is sqrt(a^2 + b^2 + c^2)
*/

::Vector3 = { x :: Real, y :: Real, z :: Real}

class Comparisons a 
where
    (*==) :: a a -> Bool
    (!=) :: a a -> Bool
    (*<) :: a a -> Bool
    (*>) :: a a -> Bool
    (*<=) :: a a -> Bool
    (*>=) :: a a -> Bool  

// Calculate the length of a Vector3
vectorLength :: Vector3 -> Real
vectorLength {x, y, z} = sqrt(x*x + y*y + z*z)

instance Comparisons Vector3 
where
    (*==) v1 v2 = vectorLength v1 == vectorLength v2
    (!=) v1 v2 = vectorLength v1 <> vectorLength v2
    (*<) v1 v2 = vectorLength v1 < vectorLength v2
    (*>) v1 v2 = vectorLength v1 > vectorLength v2
    (*<=) v1 v2 = vectorLength v1 <= vectorLength v2
    (*>=) v1 v2 = vectorLength v1 >= vectorLength v2
    	
//Start = {x = 1.0, y = 1.0, z = 1.0} *== {x = 1.0, y = 1.0, z = 1.0} // True
//Start = {x = 3.0, y = 4.0, z = 10.0} *== {x = 5.0, y = 0.0, z = 10.0} // True
//Start = {x = 1.0, y = 1.0, z = 1.0} != {x = 2.0, y = 1.0, z = 1.0} // True
//Start = {x = 1.0, y = 1.0, z = 1.0} *< {x = 2.0, y = 1.0, z = 1.0} // True
//Start = {x = 1.0, y = 1.0, z = 1.0} *> {x = 2.0, y = 1.0, z = 1.0} // True
//Start = {x = 1.0, y = 1.0, z = 1.0} *<= {x = 1.0, y = 1.0, z = 1.0} // True
//Start = {x = 1.0, y = 1.0, z = 1.0} *>= {x = 1.0, y = 1.0, z = 1.0} // True

 

 

