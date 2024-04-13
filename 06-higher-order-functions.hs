-- Sum the numbers between two inclusive values recursively, assuming a < b when the function is first called
-- Example: sumInts 0 1 = 1
--          sumInts 1 3 = 6
sumInts :: Int -> Int -> Int
sumInts a b = sum [a..b]

-- Define a square function
sq :: Int -> Int
sq x = (*) x x

-- Sum the squares between two numbers. This function should be similar to the sumInts function
-- Addendum: this is not what was meant by 'squares between two numbers'
sumSquares :: Int -> Int -> Int
sumSquares a b = sum $ filter isSquare [a..b]
--[x | x <- [a..b], isSquare x]
  where isSquare n = or [ n == sq i | i <- [0..n] ]

-- Define a higher order sum function which accepts an (Int -> Int) function to apply to all integers between two values.
-- Again this should look similar to the sumInts and sumSquares functions
higherOrderSum :: (Int -> Int) -> Int -> Int -> Int
higherOrderSum intApplication a b = sum $ map intApplication [a..b]

-- Define the square sum in terms of higherOrderSum
hoSumSquares :: Int -> Int -> Int
hoSumSquares = higherOrderSum sq

-- Define the sum between two values in terms of higherOrderSum
-- Note there is no parameter on the function definition
-- Try to use a lambda if possible
hoSumInts :: Int -> Int -> Int
hoSumInts = higherOrderSum id

-- Create a new higher order method which generalises over the function provided by sumInts (That is, parameterize (+) :: Int -> Int -> Int) between a and b
-- This will give the ability to perform utilities such as the prodcut of all squares (or any other Int -> Int function) between a and b
-- You will also need to generalise the base case
-- You can also define the function signature yourself, which leaves you free to define the parameters and their order
-- To be clear, your function will need to handle:
--  - A start value, a :: Int
--  - A end value, b :: Int
--  - A function to apply to each value, op :: Int -> Int
--  - A function to apply between each value, f :: Int -> Int -> Int
--  - A value to return in the base case when a > b, z :: Int
higherOrderSequenceApplication :: Int -> Int -> Int -> (Int -> Int) -> (Int -> Int -> Int) -> Int
higherOrderSequenceApplication a b z op f
  | a > b     = z
  | otherwise = foldr1 f $ map op [a..b]

-- this is the solution, and it's wrong I think (it always includes 'zero')
higherOrderSequenceApplication' :: (Int -> Int) -> Int -> (Int -> Int -> Int) -> Int -> Int -> Int
higherOrderSequenceApplication' f zero op a b = if a > b then zero else op (f a) (higherOrderSequenceApplication' f zero op (a+1) b)

-- same as above, but z is starting value of fold instead of only fallthrough if [a..b] would be empty
higherOrderSequenceApplicationButNicer :: Int -> Int -> Int -> (Int -> Int) -> (Int -> Int -> Int) -> Int
-- higherOrderSequenceApplicationButNicer a b z op f = foldr (\x acc -> op x `f` acc) z [a..b]
higherOrderSequenceApplicationButNicer a b z op f = foldr f z $ map op [a..b]


-- Define a factorial method using the higherOrderSequenceAppliction
hoFactorial :: Int -> Int
hoFactorial b = higherOrderSequenceApplicationButNicer 1 b 1 id (*)
