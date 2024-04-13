-- nachtrag: Rotate
rotate :: Int -> [a] -> [a]
rotate n l = drop n l ++ take n l

-- rotate in terms of split & reverse
pairToList :: (a, a) -> [a]
pairToList (x,y) = [x,y]
-- rotate2 :: Int -> [a] -> [a]
rotate2 :: Int -> [a] -> [a]
rotate2 n l = reverse (concat (map reverse (pairToList (splitAt n l))))

-- lsp told me to use concatMap
rotate3 :: Int -> [a] -> [a]
rotate3 n l = reverse (concatMap reverse (pairToList (splitAt n l)))

-- ChatGPT told me about (.) and ($)
rotate4 :: Int -> [a] -> [a]
rotate4 n l = reverse . concatMap reverse . pairToList . splitAt n $ l

-- This step doesnt really fit into my head
-- λ> reverse . pairToList . splitAt 2 $ [1,2,3,4]
-- [[3,4],[1,2]]
-- λ> concatMap reverse . pairToList . splitAt 2 $ [1,2,3,4]
-- [2,1,4,3]

-- lsp told me to eta reduce - now I don't understand the type?
rotate5 :: Int -> [a] -> [a]
rotate5 n = reverse
  . concatMap
        reverse
        . pairToList
        . splitAt n

reverse(concatMap(reverse(pairToList(splitAt(n))), l))

{-
 - For this exercise, we are dealing with a type for colours of the rainbow
 - The typeclass is defined here, and note its English spelling.
 - For more information on how this is done, look ahead to:
 - http://learnyouahaskell.com/making-our-own-types-and-typeclasses
 -
 - Have a play with the Colour in ghci, try the succ and pred functions and so on.
 -}
data Colour = Red | Orange | Yellow | Green | Blue | Indigo | Violet
    deriving (Eq, Ord, Show, Bounded, Enum)   

firstColour = minBound :: Colour
lastColour = maxBound :: Colour

-- List the colours in reverse order
allColours :: [Colour]
allColours = [maxBound, pred maxBound..minBound]

allcolours2 :: [Colour]
allcolours2 = enumFrom minBound

-- here i get told off by my lsp for using reverse on a potentially infinite list, it is clear (?) that
-- it is applied to a finite list
reverseColourOrder :: [Colour]
reverseColourOrder = reverse (enumFrom minBound)

{-
 - Mix two colours together, to produce the average value of the two.
 - Example: paintMix Orange Green = Yellow
 - If necessary, favour the "higher" value when computing the average.
 - For example: paintMix Green Violet = Indigo
 - Hint: Integer division can be performed with the quot function: quot 7 2 = 3
 -}

-- test cases
-- Red Yellow -> Orange
-- Yellow Red -> Orange
-- Red Orange -> Orange
-- Red Red -> Red

middle :: [a] -> a
middle l = l !! quot (length l) 2

-- c1 < c2 !
mixOrdered :: Colour -> Colour -> Colour
mixOrdered c1 c2 = middle [x | x <- [minBound..maxBound] :: [Colour], c1 <= x, x <= c2]

mixOrdered2 :: Colour -> Colour -> Colour
mixOrdered2 c1 c2 = middle [c1..c2]

paintMix :: Colour -> Colour -> Colour
paintMix c1 c2 = if c1 == c2 then c1 else if c1 < c2 then mixOrdered c1 c2 else mixOrdered c2 c1

paintMix2 :: Colour -> Colour -> Colour
paintMix2 c1 c2
  | c1 == c2 = c1
  | c2 < c1 = mixOrdered c2 c1
  | otherwise = mixOrdered c1 c2

paintMix3 :: Colour -> Colour -> Colour
paintMix3 c1 c2 = case compare c1 c2 of
  EQ -> c1
  GT -> mixOrdered2 c2 c1
  LT -> mixOrdered2 c1 c2
