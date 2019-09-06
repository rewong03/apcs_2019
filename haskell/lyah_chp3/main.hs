-- Basics
-- 1. You can find the type of an expression by using :t.
-- 2. The correct type of a string is [Char].
-- 3. (5 == 5) is a Bool.
-- 4. multiply :: Int -> Int -> [Char]
-- 5. multiply :: Int -> Int -> Int
-- 6. factorial :: Integer -> Intger. Unlike int, integer has no memory size, but is less efficient than int.
-- 7. prob3 :: Int -> [Char]
-- 8. prob4 :: Int -> [Int] -> [Int]
-- 9. The difference between a float and a double is that a double has twice the precision as a float.
-- 10. You can tell whether "string" is a variable or a type by doing :t "string". However, there are no 'strings' in Haskell, only [Char] and Char.
-- 11. Write the type signatures for the following functions:
   -- 1. snd :: (a, b) -> b
   -- 2. repeat :: a - > [a]
   -- 3. init :: [a] -> a
-- Class Constraints
-- 1. A class constraint can be found by doing :t func, and a type signature containing a => may be returned. If a => is returned in the signature, everything to the left of it is the constraint.
-- 2. =>
-- 3. Ord
-- 4. The show typeclass denotes that something can be represented as a string.
-- The Read Typeclass
-- 1. The read typeclass indicated that something can be converted to a string.
-- 2. You can convert "10230" into a string by doing read "10230" :: Int
-- Miscellanious Typeclasses
-- 1. The main purpose of the enum typeclass is that it is a sequential ordering and can be enumerated.
-- 2. Two other examples of the enum typeclass are letters and [LT, EQ, GT].
-- 3. The bounded typeclass would tell you that something has an upper or lower boundary.
-- 4. Using minBound.
-- Math
-- 1. The difference between 5 and (5 :: Int) is that 5 is of the Num typeclass, meaning it can be interpreted as either a float or an integer, while (5 :: Int) is of the Integral class and can only be interpreted as a whole number.
-- 2. Can you do this:
   -- 1. 3.2 * (5 :: Int). No, 3.2 is a float while 5 is a Int.
   -- 2. 5.63 * 20. Yes, 20 is a Num typeclass so it can be multiplied by a float. 
-- 3. It returns 3 times the length of the inputted list.
-- Integral vs. Floating
-- 1. fromIntegral :: (Integral a, Num b) a -> b. fromIntegral turns the typeclass of an Integral class number into a more general Num class number.
-- 2. Write a function for the quadratic formula.
quadEquat :: (Floating a, Ord a, Num a) => a -> a -> a -> a
quadEquat a b c= max (((-b) + sqrt (b ^ 2 - 4 * a * c)) / 2 * a) (((-b) - sqrt (b ^ 2 - 4 * a * c)) / 2 * a)
