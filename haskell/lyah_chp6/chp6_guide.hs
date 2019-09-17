--1. Take in a list of numbers and output a list containing all of the numbers greater than 8.
greaterThan8 list = filter (>8) list
-- 2. Take in a list of numbers and output a list containing all of the numbers between 10 and 20 (inclusive).
between1020 list = [x | x <- list, x >= 10, x <= 20]
-- 3. Take in a list of numbers. Multiply all of the numbers in a list by 10 to make the output list.
multiplyList10 list = map (*10) list
-- 4. Take in a list of lists. Arrange the list of lists according to how many elements each list has, so longer sublists go first and shorter sublists go later.
quicksortList [] = []    
quicksortList (x:xs) =     
    let smallerSorted = quicksortList (filter (\f -> (length f) <= (length x)) xs)  
        biggerSorted = quicksortList (filter (\f -> (length f) > (length x)) xs)   
    in  smallerSorted ++ [x] ++ biggerSorted  