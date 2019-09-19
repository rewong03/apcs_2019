-- 1. The main idea of the quicksort function is that a list is recursively split into smaller and smaller lists that are sorted and the conjoined.
-- 2. Write the insertBefore function that places an element before the current item with the given index in a list. Use as few built-in functions as you can. This is a thinking exercise not a practical exercise – figure out how to do it without take, drop or splitAt.
insertBefore :: Int -> a -> [a] -> [a]
insertBefore n item xs
  | n == 0 = item : xs
  | otherwise = (head xs) : (insertBefore (n - 1) item (tail xs))
-- 3. The allPermutations function takes in a list and returns a list of lists. The output contains every permutation of the input list exactly once. (Treat all of the input list elements as distinguishable; see last test case.) The permutations do not have to appear in the order given below.
allPermutations :: [a] -> [[a]]
allPermutations [a] = [[a]]
allPermutations (x:xs) = [insertBefore n x y| y <- allPermutations xs, n <- [0 .. length y]]
-- 4. Write the combinations function that takes in a number k and a list, and returns a list of lists. The output contains every distinct k item subset of the list (keep the items in the order they appear in the original list).
-- Definitely very slow but it works for unsorted lists
combinations :: (Num t, Ord a, Eq t) => t -> [a] -> [[a]]
combinations 1 list = [[item] | item <- list]
combinations k list = [x : y | x <- list, y <- (combinations (k - 1) (delete x list)), lexigraphical (x : y)]
  where lexigraphical [a] = True
        lexigraphical (x:y:xs) = x < y && lexigraphical (y:xs)
        delete item list = [x | x <- list, item /= x]
-- 5. Write the grouper function that takes in a list and produces a list of lists. Each sublist should have all of the elements in order.
-- Splits a list into a the first matching elements and the rest of the list
-- Ex. grouperHelper [1,1,2,3] ([], []) -> ([1,1], [2,3])
grouperHelper :: Eq a => [a] -> ([a], [a]) -> ([a], [a])
grouperHelper all@(a:xs) (x, y)
  | length all == 1 = (x ++ [a], y)
  | a == (head xs) = grouperHelper xs (x ++ [a], y)
  | otherwise = (x ++ [a], xs)
grouper :: Eq a => [a] -> [[a]]
grouper list
  | null list = []
  | otherwise = fst (grouperHelper list z) : grouper (snd (grouperHelper list z))
  where z = ([], [])







