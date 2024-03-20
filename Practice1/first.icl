module first
import StdEnv

//Start = "Jamal"

f :: Int -> Int

f x = x*2

//Start = f 42

//f3 :: Int -> Int

//f3 x = x  3

//Start = f3 25

f4 :: Int Real -> Real
f4 x y =  abs ((toReal x) / y )

//Start = f4 4 2.5

c :: Char -> Char
c x = x

//Start = c '%'


//Start = abs -4  // built-in


div :: Int -> Int
div x = x/2

divr :: Real -> Real
divr x = x/2.0

//Start = (div 5, divr 5.0)


divb :: Int Int -> Int
divb x y = x/y

//Start = divb 5 3


iseven :: Int -> Bool
iseven a = (a rem 2 == 0)

//Start = iseven 23  // isEven 


iseven2 :: Int -> Bool
iseven2 a = (a/2)*2 == a

//Start = iseven2 23


// granma market 5 kg apple (500.0) , 7 kg orange (800.0 )
// 10 kg potatos 150.5 . Shopping =?

granma :: Real Real Real -> Real
granma a o p = a*500.0 + o*800.0 + p*150.5

//Start = granma 5.0 7.0 10.0 


// x^2 +2x + 1     x = 5??
eq :: Int -> Int
eq x = x^2 + 2*x + 1

//Start = eq 5


// quadratic of a number n=4
qa :: Int Int -> Int
qa x n = x^n  // x*x*x*x...*x

//Start = qa 2 6


// Solve a quadratic eq. Given the coeficients find the roots!

quad :: Real Real Real Real -> Real

quad a b c x = a*x*x + b*x + c

//Start = quad 1.0 2.0 1.0 3.0


// Me 8848 K2 8611 flight Me 1000km 
// ? how far K2

ME = 8848
K2 = 8611

dist :: Int -> Int
dist x = (ME-K2) + x

//Start = dist 1000


// eq x = x^4 + 3*x^3 + 4*x^2 +x - 10  eq x = 10?
eq4 :: Int -> Int
eq4 x = x^4 + 3*x^3 + 4*x^2 +x - 10 

//Start = eq4 10 

//Start = [1, 2, 3, 4, 5, 6, 7]!!4