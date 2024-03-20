module zjuar3_pt1

import StdEnv 


// ZJUAR3 Jamal Mammadov
/*
Write a  function called calculate_grade that takes an integer as input called
score and returns the corresponding string grade based on the following grading
scale:
If score is less than 60, the function should return "F" (Fail).
If score is between 60 and 69, the function should return "D" (Pass).
If score is between 70 and 79, the function should return "C" (Satisfactory).
If score is between 80 and 89, the function should return "B" (Good).
If score is 90 or higher, the function should return "A" (Excellent).
Make sure to provide the correct grade based on the given score input.
If the input score is not in the range of 0 to 100 (inclusive), return "Invalid input"
*/

calculate_grade :: Int -> String
calculate_grade x
| 0 <= x && x < 60 = "F"
| 60 <= x && x <= 69 = "D"
| 70 <= x && x <= 79 = "C"
| 80 <= x && x <= 89 = "B"  
| 90 <= x && x <= 100 = "A"
= abort "Invalid input"

//Start = calculate_grade 59 // "F"
//Start = calculate_grade 60 // "D"
//Start = calculate_grade 69 // "D"
//Start = calculate_grade 70 // "C"
//Start = calculate_grade 89 // "B"
//Start = calculate_grade 90 // "A"
//Start = calculate_grade 120 // "Invalid input"
//Start = calculate_grade -28 // "Invalid input"