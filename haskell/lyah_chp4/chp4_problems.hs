-- Excercises
-- 1. First Third. Return the sum of the first and third elements in the list. Example: firstThird [10,30,50,90] == 60
firstThird :: Num a => [a] -> a
firstThird numList
  | length numList < 3 = error "List is too short!"
  | otherwise = numList !! 0 + numList !! 2
-- 2. Take a list of items that can be compared for equality (Eq a) and reduce repeats to a single element. Only reduce repeats that are next to each other in the list.
noTwins :: Eq a => [a] -> [a]
noTwins itemList@(a:b)
  | null b = [a]
  | a == head b = noTwins b
  | otherwise = [a] ++ noTwins b
-- 3. (CodingBat sum67.) Return the sum of the integers in the array, except ignore sections of numbers starting with a 6 and extending to the next 7 (every 6 will be followed by at least one 7). Return 0 for no numbers.
sum67 :: (Num a, Eq a) => [a] -> a
sum67 numList@(a:b)
  | null b = a
  | a == 6 = sum67 (removeUpTo7 b)
  | otherwise = a + sum67 b
  where removeUpTo7 numList@(a:b)
          | a == 7 && null b = [0]                              
          | a == 7 = b
          | otherwise = removeUpTo7 b
-- 4. Make a newFibonacci function that takes in the two starting values for the Fibonacci sequence (usually 1 and 1) and then the term in the sequence that you want, and computes that term in the sequence.
newFibonacci :: (Num a, Eq a) => a -> a -> a -> a
newFibonacci t1 t2 n
  | n == 1 = t1
  | n == 2 = t2
  | otherwise = newFibonacci t1 t2 (n - 1) + newFibonacci t1 t2 (n - 2)
-- 5. Block game
-- Generates all possible combinations of the given list of word tuples and returns them as a list of strings
join :: [[Char]] -> [Char]
-- Joins a list of strings into a single string
join strList
          | null strList = ""
          | otherwise = (head strList) ++ join (tail strList)
generateSets :: [([Char], [Char])] -> [[Char]]
-- Takes a set of word 2-tuples and produces every possible combination of the first and second items of each tuple
generateSets wordTuple = [generateSet x wordTuple| x <- [0 .. (2 ^ length wordTuple - 1)]]
  where generateSet x wordTuple
          | maxChar == 0 = join [fst y| y <- wordTuple]
          | x `mod` 2 == 1 = snd (head wordTuple) ++ generateSet (x `div` 2) (tail wordTuple)
          | otherwise = fst (head wordTuple) ++ generateSet (x `div` 2) (tail wordTuple)
            where maxChar = length wordTuple
--uniqueLetters :: [[Char]] -> [Char]
-- Removes multiple instances of a letter
uniqueLetters strList = [x | x <- ['a' .. 'z'], x `elem` joinedList]
  where joinedList = join strList
-- Count the number of each letter in a list of words
blockGame wordTuple = maxLetters (countLetters (generateSets wordTuple))
  where maxLetters letterCounts = [maximum [letterCount !! x| letterCount <- letterCounts]| x <- [0 .. 25]]
        countLetters strList = [countLettersInWord str| str <- strList]
          where countLettersInWord str = [countLetter x str uniqueStr| x <- ['a' .. 'z']]
                  where countLetter letter str uniqueStr
                          | not (letter `elem` uniqueStr) = 0
                          | otherwise = length (filter (== letter) str)
                uniqueStr = uniqueLetters strList
bigList = [(['q' .. 'z'], ['q' .. 'z'])| x <- [1 .. 20]]
-- Games
-- 1. Laser Tag I. You are playing laser tag on the integer coordinate plane. You stand at (x0,y0) and fire your laser parallel to one of the axes. We will write the direction using unit coordinates, so (1,0) will be the positive x-direction. The coordinates of the other players are in a list. Assuming that you hit a player, report the coordinates of the player who is hit.
laserTag :: (Eq a, Num a) => (a, a) -> (a, a) -> [(a, a)] -> (a, a)
laserTag playerPos shotDir playerList
  | (addTuple playerPos shotDir) `elem` playerList = addTuple playerPos shotDir
  | otherwise = laserTag (addTuple playerPos shotDir) shotDir playerList
  where addTuple (a, b) (c, d) = (a + c, b + d)
-- 2. Laser Tag II. (Save for later if needed.) We continue to play the laser tag game, except now there are both people and double-sided mirrors oriented at 45 degrees to the coordinate axes. Represent the objects by triples (x,y,c) where c is a character * for a person, / or \ for a mirror (depending on the orientation). Assume that eventually the laser will hit someone (maybe you?!). Find the coordinates of the person who gets hit.
-- For this case < will represent \ and > will be /
laserTagMirrors :: (Int, Int) -> (Int, Int) -> [(Int, Int, Char)] -> (Int, Int)
laserTagMirrors playerPos shotDir playerList
  | isPlayer (addTuple playerPos shotDir) playerList = (addTuple playerPos shotDir)
  | leftMirror playerPos playerList = laserTagMirrors (addTuple playerPos (changeDir '<' shotDir))  (changeDir '<' shotDir) playerList
  | rightMirror playerPos playerList = laserTagMirrors (addTuple playerPos (changeDir '>' shotDir)) (changeDir '>' shotDir) playerList
  | otherwise = laserTagMirrors (addTuple playerPos shotDir) shotDir playerList
  where addTuple (a, b) (d, e) = (a + d, b + e)
        leftMirror (x, y) playerList
          | (x, y, '<') `elem` playerList = True
          | otherwise = False
        rightMirror (x, y) playerList
          | (x, y, '>') `elem` playerList = True
          | otherwise = False
        isPlayer (x, y) playerList
          | (x, y, '*') `elem` playerList = True
          | otherwise = False
        changeDir mirror (x, y)
          | mirror == '<' = (-y, x)
          | mirror == '>' = (y, x)
everyFour = [31, 35 .. 431]
oddSquares = [x ^ 2 | x <- [0 .. 31], odd x]
linePoints = [(x, (3 * x - 12))| x <- [0.1, 0.2 .. 10]]
prob4 numList = [x | x <- numList, 200 <= x, x <= 300]
        
        



