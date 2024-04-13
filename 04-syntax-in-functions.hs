-- This function should print a single digit number as English text, or "unknown" if it's out of the range 0-9
englishDigit :: Int -> String
englishDigit 1 = "one"
englishDigit 2 = "two"
-- I'm lazy
englishDigit x = "unknown"

englishDigit2 :: Int -> String
englishDigit2 d
  | d < 0 = "unknown"
  | d > 9 = "unknown"
  | otherwise = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] !! d

-- given a tuple, divide fst by snd, using pattern matching. 
-- it should return undefined for division by zero
divTuple :: (Eq a, Fractional a) => (a, a) -> a
divTuple (x, 0) = undefined
divTuple (x, y) = x / y

-- if the first three numbers in a list are all zero, return True
threeZeroList :: [Int] -> Bool
threeZeroList (0:0:0:_) = True
threeZeroList _ = False
