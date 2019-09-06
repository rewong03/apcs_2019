-- Section A
-- 1. The right way to write five times negative three is 5 * (-3), the incorrect way is 5 * -3
-- 2. and: &&, or: ||, not: not, equals: ==, not equals: /=
-- 3. Function calling is at the beginning of the order of operations, so functions will be evaluated first. An example of a problem this might create is that the equation f 5 * 2 with f being some function will be evaluated as (f 5) * 2 rather than f (5 * 2).
-- 4. hypsquare function that computes the square of the length of the hypotenuse of a right triangle with legs a and b.
hypsquare :: Int -> Int -> Int
hypsquare a b = a ^ 2 + b ^ 2
-- 5. The apostrophe character denotes that a variable or function is either a non-lazy function or it is a slight modification of another variable or function.
-- 6. Apostrophe's are allowed in function and variable names.
-- 7. In haskell you can simply declare list_example = ["list", "of", "stuff"].
-- 8. Strings can be concatenated by using ++ (ex. str1 ++ str2 returns str1str2).
-- 9. learnMore !! 5 returns an error as you're attempting to return the fifth item of a 4 item list.
learn = ["this", "is", "all", "good"]
learnMore = "hopefully": learn
-- 10. first: head, rest: tail, length: length, empty?: null
-- 11. What are the signatures and purposes of these functions?
      -- 1. init :: [a] -> [a]. init takes a list and returns a list of all the values excluding the last item in the inputted list.
      -- 2. last :: [a] -> a. last takes a list and returns the last item from within the inputted list.
      -- 3. take :: Integer -> [a] -> [a]. take returns the first n number of values from the inputted list.
      -- 4. drop :: Integer -> [a] -> [a]. drop returns the inputted list with the first n values removed. 
-- 12. 5 `elem` xs
-- 13. Write summaries for the following functions:
      -- 1. reverse returns a list with reversed values.
      -- 2. maximum returns the largest value in a list.
      -- 3. minimum returns the smallest value in a list.
      -- 4. sum returns the sum of all values in a list.
      -- 4. product returns the product of all values in a list.
-- Section B
-- 1. Create a list of inntegers from 1 to 1000 (inclusive).
one_to_thousand = [1 .. 1000]
-- 2. Create a list of letters from 'm' to 'a' in that order.
m_to_a = reverse ['a' .. 'm']
-- 3. Write signatures and purposes for the following functions:
     -- 1. cycle :: [a] -> [a]. cycle takes a list and returns a list with infinite elements, repeating the inputted list
     -- 2. repeat :: a -> [a]. repeat takes an item and returns an infinite list with just that item.
     -- 3. replicate :: Integer -> a -> [a]. replicate returns a list with n number of item.
-- 4. Write a function that removes all vowels from a word.
remove_vowel str = [x| x <- str, not(x `elem` ['a', 'e', 'i', 'o', 'u'])]
-- 5. The main differences between tuples and lists is that lists are homogenous while tuples aren't, and tuples have an immutable size.
-- 6. Write signatures and purposes for the following functions:
   -- 1. fst :: (a, b) -> a. fst returns the first item in a 2-tuple.
   -- 2. snd :: (a, b) -> b. snd returns thr second item in a 2-tuple. 
   -- 3. zip :: [a] -> [b] -> [(a, b)]
-- 7. Write a function that takes a list of words and returns a list of tuples (word, word length).
wordLen :: [[Char]] -> [([Char], Int)]
wordLen strList = [(x, length x)|x <- strList]
-- 8. Write a function that takes a list of strings and returns the first item, second item doubled, third item triples, etc.
amplify :: [a] -> [[a]]
amplify strList= [replicate (fst x) (snd x)|x <- zip [1 .. length strList] strList]
