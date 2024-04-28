import Control.Applicative
import Data.Monoid

-- import Test.QuickCheck.Classes

-- We can use the following type to simulate our own list
data List a = Empty | Value a (List a) deriving (Show, Eq)

-- Make the list a Functor
instance Functor List where
  fmap _ Empty = Empty
  fmap f (Value x xs) = Value (f x) (fmap f xs)

-- Write a function which appends one list on to another
combineLists:: List a -> List a -> List a
combineLists Empty b = b
combineLists a Empty = a
combineLists (Value x xs) b = Value x (combineLists xs b)

-- Make our list a Monoid
instance Semigroup (List a) where
  (<>) = combineLists

instance Monoid (List a) where
  mempty = Empty

-- Make our list an Applicative
instance Applicative List where
  pure x = Value x Empty
  Empty <*> l = Empty
  (Value f fs) <*> l = fmap f l <> (fs <*> l)

-- Make sure that the List obeys the laws for Applicative and Monoid
-- also check Test.QuickCheck.Classes.applicative

-- Monoid: https://hackage.haskell.org/package/base-4.19.1.0/docs/Prelude.html#t:Monoid
-- Applicative: https://hackage.haskell.org/package/base-4.19.1.0/docs/Control-Applicative.html
test :: (Eq a) => [List a] -> Bool
test l = and ([testUnary] <*> pure l)
  where testUnary l = and $ [f x | f <- [identityMonoid, identityApplicative, identityApplicative'], x <- l]
          where
            identityMonoid x = (x <> mempty == x) && (mempty <> x == x)
            identityApplicative x = (pure id <*> x) == x
            identityApplicative' x = (id <$> x) == x
        testBinary l = and $ [f x y | f <- [], x <- l, y <- l]
          where
            -- homomorphism pure f <*> pure x == pure (f x)  -- needs different types f, x :(
        testTernary l = and $ [f x y z | f <- [associativity], x <- l, y <- l, z <- l]
          where
            associativity x y z = ((x <> y) <> z) == (x <> ( y <> z))
            -- composition u v w = ((.) <$> u <*> v <*> w) == (u <*> (v <*> w)) -- u v w need to be functions -> how to actually check equality between function? What constraing on a?
 
composition u v w = ((.) <$> u <*> v <*> w) == (u <*> (v <*> w))

-- Create some lists of numbers of different lengths such as:
twoValueList = Value 10 $ Value 20 Empty

-- Use <$> on the lists with a single-parameter function, such as:
plusTwo = (+2)

a = (+2) <$> twoValueList

-- Use <$> and <*> on the lists with a binary function
b = (*) <$> twoValueList <*> twoValueList

-- Create some lists of binary functions
twoFunctionsList = Value (+) $ Value (*) Empty

-- Use <*> on the binary functions list and the number lists
c = twoFunctionsList <*> twoValueList <*> twoValueList
