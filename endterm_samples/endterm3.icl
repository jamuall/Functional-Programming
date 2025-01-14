module endterm3
import StdEnv

/*---------------------------------------------------------------
-- Functional Programming end-term

-- This solution was submitted and prepared by
-- <name, neptun> for the end-term programming assignment of
-- the Functional Programming course.

-- I declare that this solution is my own work.

-- I have not copied or used third-party solutions.

-- I have not passed my solution to my classmates, neither made it public.

-- Students' regulation of Eotvos Lorand University (ELTE Regulations Vol. II. 74/C.)
-- states that as long as a student presents another student’s work -
-- or at least the significant part of it - as his/her own performance,
-- it will count as a disciplinary fault.

-- The most serious consequence of a disciplinary fault can be dismissal
-- of the student from the University.
*/

 

//---------------

/* 1. Arrays. 10 points
Given an array of integers, remove the elements that have even occurances in the array.
*/
occurence :: Int [Int] -> Int
occurence _ [] = 0
occurence num ls = length[a\\a<-ls | a == num]

removeOcc :: {Int} -> {Int}
removeOcc arr = {a\\a<-ls | isOdd (occurence a ls)}
where
	ls = [a\\a<-:arr]
	
//Start = removeOcc {1,1,2,2,2,3,3,4,5,6,6,6,6} //{2,2,2,4,5}
//Start = removeOcc {1,1} // {}
//Start = removeOcc {1,1,1} // {1,1,1}
//Start = removeOcc {1,1,2,2,3,4,5} // {3,4,5}

//---------------

/* 2. Generator. 10 points
Given a positive integer value n, generate an array that for n=10
has as elements 1,2,2,3,3,3,4,4,4,4,...,10,...,10.
*/

generate :: Int -> {Int}
generate 0 = {}
generate num = {a \\a<-[1,2..num], b<-[1,2..a]}
//Start :: {Int} // this is needed
//Start = generate 10 // {1,2,2,3,3,3,4,4,4,4,...,10,...,10}
//Start = generate 4 // {1,2,2,3,3,3,4,4,4,4}

//---------------

:: City = BUDAPEST | GYOR | DEBRECEN
:: Product = {productName :: String , price :: Real}
:: Shop = {shopName :: String , products :: {Product}, location :: City}

meat = {productName ="meat" ,price= 5000.123}
fruits={productName ="fruits" , price=2000.123}
vegetables = {productName ="vegetables",price=1700.50}

aldi = {shopName = "aldi" , products = {meat,fruits,vegetables} ,location = BUDAPEST}
spar = {shopName = "spar" , products = {{productName ="meat" ,price= 4500.0},fruits,vegetables} , location = GYOR}
lidl = {shopName = "lidl" , products = {meat,fruits,vegetables} ,location = BUDAPEST}
abc = {shopName = "abc" , products = {{productName ="meat" ,price= 4500.0},{productName ="fruits" ,price= 1700.0},{productName ="vegetables" ,price= 1500.0}} , location = GYOR}

//---------------

/* 3. Shops. 10 points
Given an array of shops, all the shopes have the same products but with different
price, return a tuple containing the name of the cheapest shop and it's location.
the cheapest shop is the shop in the array whose sum of its products prices
is the smallest in the array.
Note : if there is more than one cheap shop in the array, return the first one.
Assume that the given array is not empty.
*/
calcprod :: {Product} -> Real
calcprod arr = sum[product.price\\product<-:arr]
//Start = calcprod {meat,fruits,vegetables}

cheapestShop :: {Shop} -> (String,City)
cheapestShop shoparray = hd[(shop.shopName,shop.location)\\shop<-shopls]
where
	shopls = sortBy (\s1 s2 = (calcprod s1.products) < calcprod s2.products) [shop\\shop<-:shoparray]
//Start = cheapestShop {aldi,spar,lidl,abc} // ("abc",GYOR)
//Start = cheapestShop {aldi,spar} // ("spar",GYOR)
//Start = cheapestShop {lidl,aldi} // ("lidl",BUDAPEST)

//---------------

:: Person = {name :: String, age :: Int , numbers :: {Int}}
abdullah :: Person
abdullah = {name = "Abdullah", age = 13 , numbers = {-4,3,2,1} }
abood :: Person
abood = {name = "abood", age = 12 , numbers = {3,-6,5,-2,1} }
othman :: Person
othman = {name = "othman", age = 12 , numbers = {-5,4,-2,3,1} }
mohammed :: Person
mohammed = {name = "Mohammed" , age = 18 , numbers = {-5,4,-2,3,1,-1,-6,-1,0,5}}

//---------------

/* 4. Vectors. 30 points BE AWARE THAT THIS TASK CONSISTS OF MULTIPLE SMALL TASKS.
A vector in programming is a dynamic implementation of the Array data structure
Create the pushBack, pushFront, remove, indexOf, and swap operations for this type.
*/

:: Vector a :== [a]

/* 4.1 5 points
pushBack is a function that takes a vector and an element and
adds the element to the end of the vector
*/

pushBack :: (Vector a) a -> (Vector a)
pushBack vct num = vct ++ [num]
//Start = pushBack [1,2,3] 4 //[1,2,3,4]
//Start = pushBack [1,0,213] 10000 //[1,0,213,10000]

/* 4.2 5 points
pushFront is a function that takes a vector and an element and
adds the element to the beginning of the vector
*/

pushFront :: (Vector a) a -> (Vector a)
pushFront vct num = [num] ++ vct
//Start = pushFront [1,0,213] 10000 //[10000,1,0,213]
//Start = pushFront [1,2,3] 4 //[4,1,2,3]

/* 4.3 5 points
remove is a function that takes a vector and an element and
removes the element from the vector
If it exists, and returns it. Otherwise it returns an error.
*/

remove :: (Vector a) a -> (Vector a) | Eq, Ord a
remove vct num
| n >= 1 = [v\\v<-vct | v <> num]
| n == 0 = abort "Element does not exist"
where
	n = length [v\\v<-vct | v == num]
//Start = remove [1,2,3] 2 //[1,3]
//Start = remove [1,0,213] 10000 //"Element does not exist"

/* 4.4 5 points
indexOf is a function that takes a vector and an element
and returns the element's index in the vector (counting from 0)
If it exists otherwise it returns an error.
*/

indexOf :: (Vector a) a -> Int | Eq, Ord a
indexOf vct num 
| n >= 1 = hd[c\\v<-vct & c<-[0,1..] | v == num]
| n == 0 = abort "Element does not exist"
where
	n = length [v\\v<-vct | v == num]
//Start = indexOf [1,2,3] 2 // 1
//Start = indexOf [1,0,213] 10000 //"Element does not exist"

/* 4.4 10 points
swap is a function that takes a vector and two elements and swaps the two elements in the vector
if they both exist, otherwise it returns an error
*/

swap :: (Vector a) a a -> (Vector a) | Eq, Ord a
swap vct a b
|isMember a vct && isMember b vct = insertAt indexb tempb (insertAt indexa tempa ls)
= abort "Element does not exist"
where 
	indexa = indexOf vct a
	indexb = indexOf vct b
	tempa = b
	tempb = a
	ls = [ v \\ v<-vct | v <> a && v <> b]
	
//Start = insertAt 2  3 [1,2,4,5,6]
//Start = swap [1,2,4,5,6,3,888,9,7] 1 3 // [3,2,4,5,6,1,888,9,7]
//Start = swap [1,0,213] 10000 0 //"Element does not exist"

//---------------

/* 5. Triples. 10 points
For a given n generate a list of triple pairs with numbers for 1 to n,
their cubes and triples.
*/

triples :: Int -> [(Int,Int,Int)]
triples num = [(n, n^3, n*3)\\n<-[1,2..num]]
//Start = triples 2 // [(1,1,3),(2,8,6)]
//Start = triples 4 // [(1,1,3),(2,8,6),(3,27,9),(4,64,12)]

//---------------

/* 6. Merge class. 10 points
Create a class Merge which has operations sorted, mess and has the neutral element Empty.
The sorted and mess are doing the following operations:
sorted -> merges sorted lists and returns sorted list.
 If a list is not sorted,
	replace it with empty list and merge.

mess -> merges lists from the beginning of the first one and follows from the last of the second.

for instance: mess [1,2,3,5] [9,8,10] = [1,10,2,8,3,9,5]

Empty -> empty list

After that create an instance for [Int].

*/
//Start = flatten[[a]++[b]\\a<-ls1 & b<-reverse ls2 ]
class Merge a
where
	sorted :: a a -> a
	mess :: a a -> a
	Empty :: a
	
isSorted :: [a] -> Bool | Eq, Ord a
isSorted a = sort a == a
	
mymerge :: [a] [a] -> [a] | Eq, Ord a
mymerge ls1 ls2
|a == 0 = reverse ls2
|b == 0 = ls1
|a == b = ls
|a > b = ls ++ (drop b ls1)
|a < b = ls ++ (drop a ls2)
where
	a = length ls1
	b = length ls2
	ls = flatten[[a]++[b]\\a<-ls1 & b<-reverse ls2 ]
	
instance Merge [Int]
where
	sorted a b
		|isSorted a && isSorted b = sort(a ++ b)
		|isSorted a && not(isSorted b) = a
		|isSorted b && not(isSorted a) = b
		|not(isSorted b) && not(isSorted a) = []
	mess a b = mymerge a b
	Empty = []

//Start = mess [1,2,3,5] [9,8,10] // [1,10,2,8,3,9,5]
//Start = sorted [1..10] [7..15] // [1,2,3,4,5,6,7,7,8,8,9,9,10,10,11,12,13,14,15]
//Start = sorted [3..7] Empty // [3,4,5,6,7]
//Start = sorted Empty [1,3,7,4,2] // []
//Start = mess Empty [1..10] // [10,9,8,7,6,5,4,3,2,1]

//---------------

/* 7. BST. 10 points
Write a binary search tree type. Build from an arbitrary list a binary search tree
then collect from the tree the elements (which by this would be sorted)
*/
:: Tree a = Node a (Tree a) (Tree a) | Leaf

bsearch :: [Int] -> (Tree Int)
bsearch [] = Leaf
bsearch ls = Node (ls !! middle) (bsearch fst) (bsearch snd)

where 
	middle = length ls / 2
	fst = take middle ls 
	snd = drop (middle + 1) ls

bcollect :: (Tree Int)-> [Int]
bcollect Leaf = []
bcollect (Node a l r) = bcollect l ++ [a] ++ bcollect r

//---------------

/* 8. FlexTree. 10 points
Flex Tree can have 4 types of nodes: Ternary, Binary, Unary and Terminal. As
names suggest these nodes have 3, 2, 1 and 0 children nodes repsectively.
Terminal nodes do not store any value, they indicate end of the tree.

Write a function that takes a FlexTree as an argument and converts
it to the list with following rules:
* TerminalNode should be converted to empty list.
* UnaryNode's child subt ree should be converted to the list and this node's
value should be appended from front.
* BinaryNode's left and right children subtrees should be converted in order
and this node's value should be inserted after the left subtree values and
before the right subtree values.
* TernaryNode's left, mid and right children subtrees should be converted in
order and this node's value should be inserted after the left subtree values
and before the mid subtree values.

For example, if we have a FlexTree:
(TernaryNode 1)
/ | \
(BinaryNode 2) TerminalNode (UnaryNode 3)
/ \ |
TerminalNode (UnaryNode 4) (BinaryNode 5)
| / \
TerminalNode TerminalNode TerminalNode

After converting it to the list with these rules we get:
[2, 4, 1, 3, 5]
*/

 

:: FlexTree a = TernaryNode a (FlexTree a) (FlexTree a) (FlexTree a)
                            | BinaryNode a (FlexTree a) (FlexTree a)
                            | UnaryNode a (FlexTree a)
                            | TerminalNode

ftree1 = UnaryNode 1 (BinaryNode 2 TerminalNode TerminalNode)
ftree2 = BinaryNode 1 TerminalNode ftree1
ftree3 = TernaryNode 3 TerminalNode (UnaryNode 3 TerminalNode) (UnaryNode 3 TerminalNode)
ftree4 = TernaryNode 1 ftree2 TerminalNode (BinaryNode 2 (TernaryNode 1 TerminalNode TerminalNode TerminalNode) (BinaryNode 2 ftree2 ftree3))

 

flexTreeToList :: (FlexTree a) -> [a]
flexTreeToList TerminalNode = []
flexTreeToList (UnaryNode val child) = [val] ++ flexTreeToList child
flexTreeToList (BinaryNode val left right) = flexTreeToList left ++ [val] ++ flexTreeToList right
flexTreeToList (TernaryNode val left mid right) = flexTreeToList left ++ [val] ++ flexTreeToList mid ++ flexTreeToList right

//Start = flexTreeToList TerminalNode // []
//Start = flexTreeToList ftree1 // [1,2]
//Start = flexTreeToList ftree2 // [1,1,2]
//Start = flexTreeToList ftree3 // [3,3,3]
//Start = flexTreeToList ftree4 // [1,1,2,1,1,2,1,1,2,2,3,3,3]

//---------------

