-- Sections 4.1-4.3
-- 1. A pattern starts out with “dog”,“cat”, “cat dog”, “cat dog cat”, “cat dog cat cat dog”, “cat dog cat cat dog cat dog cat” and so on - each new string is created by appending the previous two with a space in between them. Write a function that takes in the number of the word in the sequence and returns the word, starting with 1 is “dog”.
catDog 1 = "dog"
catDog 2 = "cat"
catDog n = catDog (n - 1) ++ " " ++ catDog (n - 2)
-- 2. Write the function scalar_mult :: (Num a) => a -> (a,a) -> (a,a) that multiplies each number in a coordinate pair by the same number, so scalar_mult 5 (1,3) == (5,15).
scalar_mult :: (Num a) => a -> (a,a) -> (a,a)
scalar_mult n (x, y) = (n * x, n * y)
-- 3. Write a signature and function body for pair_prod that takes in a list of ordered pairs of numbers and returns a list of the products of the pairs, so pair_prod [(3,5), (10,0), (7,3)]===[15,0,21].
pair_prod :: (Num a) => [(a, a)] -> [a]
pair_prod pairList = [fst x * snd x| x <- pairList]
-- 4. someFunc (x: xs)
-- 5. error "Some error message"
-- 6. No, error just takes a string
-- 7. A function two_of takes in a list of showable numbers and puts out a String that says “__ is item 2 of __“. For example two_of [10,25,50] == "25 is item 2 of [10,25,50]". Write the signature and function definition for two_of.
two_of :: Show a => [a] -> [Char]
two_of [] = error "No items in list!"
two_of [_] = error "Less than one item in the list!"
two_of numList = "The second item of " ++ show numList ++ " is " ++ show (numList !! 1)
-- 8. Write the type signature and function definition for double_double which takes in any list and produces the same kind of list. The double_double function makes a list where the first term of the input appears twice, follwed by the whole input twice, so double_double [5,10,15] == [5,5,5,10,15,5,10,15]
double_double :: [a] -> [a]
double_double [] = error "No items in list!"
double_double [x] = [x| y <- [1 .. 6]]
double_double all@(x: _) = [x, x] ++ all ++ all
-- 9. Write a function that takes in a number and returns “small” if the number is below 10, “medium” if the number is between 10 and 20 (inclusive), and “large” if the number is larger than 20. Do not use the if...then...else construct.
numSize n
  | n < 10 = "small"
  | n <= 20 = "medium"
  | otherwise = "large"
-- 10. Define the function burgers_of using an infix definition so that
-- 3 `burgers_of` "beef" == "Three hamburgers"
-- 5 `burgers_of` "soy"  == "Five tofu burgers"
-- any other set of inputs gives "No thanks, not hungry"
a `burgers_of` b
  | a == 3 && b == "beef" = "Three hamburgers"
  | a == 5 && b == "soy" = "Five tofu burgers"
  | otherwise = "No thanks, not hungry"
-- 11. Repeating yourself.
   -- 1. weight / height ^ 2
   -- 2. The author says it's as desirable as getting kicked in the head.
   -- 3. The author says to use `where` instead of repeating a segment.
-- Sections 4.4-4.5
-- 1. The difference between where you can see terms from let bindings and where bindings is that let bindings can only be seen from within a guard and are very local and where bindings span across multiple guards in a function. 
-- 2. This means that let bindings are independent and can be placed anywhere.
-- 3. Introducing functions in a local scope, binding local variables, inside list comprehensions, etc.
-- 4. You can define two different variables using let by using pattern matching (e.g. let (a, b) = (1, 2) in some_func) or by using semicolons to separate values (e.g. let a = 1; b = 2 in some_func)
-- 5. They are the same but case statements can be used anywhere while pattern matching can only be done during function declarations.
