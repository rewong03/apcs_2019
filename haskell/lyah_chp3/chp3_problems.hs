-- Chapter 3 Problem Set
-- 1. Find the types for the operations below:
   -- 1. (/): Float typeclass.
   -- 2. (5::Int) / (10::Int): Can't be computed as (/) cannot take Int typeclasses.
   -- 3. (5::Int) / (10::Float): Can't be computed because there is a mixing of typeclasses.
   -- 4. (5::Float) / (10::Float): Float typeclass.
-- 2. Write a function to average the numbers in a list.
avg a = sum a / fromIntegral (length a)
-- 3. Write a signature and function for add10Word which reads an integer from a string and adds 10 to it.
add10Word :: (Read a, Num a) => String -> a
add10Word numStr = read numStr + 10
-- 4. Write a signature and function body for numberInSentence, which takes a number and returns the sentence “I have ___ pounds of flour.”
numberInSentence :: Show a => a -> [Char]
numberInSentence num = "I have " ++ show num ++ " pounds of flour"
-- 5. Write a signature and function body for doubleDebt which takes a String and puts out a Float. The String contains a floating point number that represents how much money someone owes, and the Float output is twice that much.
doubleDebt :: String -> Float
doubleDebt debt = (read debt :: Float) * 2
-- 6. Write a signature and function body for sampleStdDev that finds the sample standard deviation of a list of (the right kind of) numbers.
sampleStdDev :: Floating a => [a] -> a
sampleStdDev numList = sqrt (sum [(a - sum numList / fromIntegral (length numList)) ^ 2|a <- numList] / fromIntegral (length numList - 1))
-- 7.  Write the function median to find the median of a list of items that can be ordered. (Use the type Int, if you prefer.) Assume the list is sorted.
median :: Fractional a => [a] -> a
median [x] = x
median [x, y] = (x + y) / 2
median numList = median (tail (init numList))