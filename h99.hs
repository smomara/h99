import Control.Arrow ((&&&))
import System.Random (getStdGen, randomRs)
import Data.List (nub, tails, sortBy)
import Data.Function (on)
import qualified Data.Map as M
import Data.Map (Map)

-- | Problem 1
-- Find the last element of a list
myLast :: [a] -> a
myLast [] = error "empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

-- | Problem 2
-- Find hte last-but-one (or second-last) element of a list
myButLast :: [a] -> a
myButLast (x:y:[]) = x
myButLast (_:xs) = myButLast xs
myButLast _ = error "list too short"

-- | Problem 3
-- Find the K'th element of a list
-- The first element on the list is number 1
elementAt :: [a] -> Int -> a
elementAt [] _ = error "out of bounds"
elementAt (x:_) 1 = x
elementAt (x:xs) n = elementAt xs (n-1)

-- | Problem 4
-- Find the number of elements in a list
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- | Problem 5
-- Reverse a list
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- | Problem 6
-- Find out whether a list is a palindrome
isPalindrome :: Eq a => [a] -> Bool
isPalindrome = uncurry (==) . (reverse &&& id)

-- | Problem 7
-- Flatten a nested list structure
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List xs) = concatMap flatten xs

-- | Problem 8
-- Eliminate consecutive duplicates of list elements
compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:xs) = x : compress (dropWhile (==x) xs)

-- | Problem 9
-- Pack consecutive duplicates of list elements into sublists
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x:ys) : pack zs
  where (ys,zs) = span (==x) xs

-- | Problem 10
-- Run-length encoding of a list
encode :: Eq a => [a] -> [(Int, a)]
encode = uncurry zip . (map length &&& map head) . pack

-- | Problem 11
-- Modified run-length encoding
data Encoded a = Single a | Multiple Int a
  deriving Show

encodeModified :: Eq a => [a] -> [Encoded a]
encodeModified = map toEncoded . encode
  where
    toEncoded (1, x) = Single x
    toEncoded (n, x) = Multiple n x

-- | Problem 12
-- Decode a run-length encoded list
decodeModified :: [Encoded a] -> [a]
decodeModified = concatMap decoder
  where
    decoder (Single x) = [x]
    decoder (Multiple n x) = replicate n x

-- | Problem 13
-- Run-length encoding of a list
encodeDirect :: Eq a => [a] -> [Encoded a]
encodeDirect [] = []
encodeDirect (x:xs) = (if n == 1 then Single x else Multiple n x) : encodeModified zs
  where
    (ys,zs) = span (==x) xs
    n = 1 + length ys

-- | Problem 14
-- Duplicate the elements of a list
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x : x : dupli xs

-- | Problem 15
-- Replicate the elements of a list a given number of times
repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = (replicate n x) ++ (repli xs n)

-- | Problem 16
-- Drop every N'th element from a list
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = [x | (x, i) <- zip xs [1..], i `mod` n /= 0]

-- | Problem 17
-- Split a listinto two parts; the length of the first part is given
split :: [a] -> Int -> ([a], [a])
split xs n = (take n xs, drop n xs)

-- | Problem 18
-- Extract a slice from a list
slice :: [a] -> Int -> Int -> [a]
slice xs beg end = [x | (x, i) <- zip xs [1..], beg <= i && i <= end]

-- | Problem 19
-- Rotate a list N places to the left
rotate :: [a] -> Int -> [a]
rotate xs n = (drop i xs) ++ (take i xs)
  where i = n `mod` length xs

-- | Problem 20
-- Remove the K'th element from a list
removeAt :: Int -> [a] -> (a, [a])
removeAt _ [] = error "index out of bounds"
removeAt n (x:xs)
  | n <= 0    = error "index must be positive"
  | n > length xs = error "index out of bounds"
  | n == 1    = (x, xs)
  | otherwise = let (removed, rest) = removeAt (n-1) (xs)
                in (removed, x:rest)

-- | Problem 21
-- Insert an element at a given position into a list
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs i
  | i <= 0 = error "index out of bounds"
  | i > length xs + 1 = error "index out of bounds"
  | otherwise = take (i-1) xs ++ [x] ++ drop (i-1) xs

-- | Problem 22
-- Create a list containing all integers within a given range
range :: Int -> Int -> [Int]
range x y
  | x > y = []
  | x == y = [x]
  | x < y = x : range (x+1) y

-- | Problem 23
-- Extract a given number of randomly selected elements from a list
rnd_select :: [a] -> Int -> IO [a]
rnd_select xs n = map (xs !!) <$> indices
    where indices = take n . nub . randomRs (0, length xs - 1) <$> getStdGen

-- | Problem 24
-- Draw N different random numbers from the set 1..M
diff_select :: Int -> Int -> IO [Int]
diff_select n m = take n . nub . randomRs (0, m) <$> getStdGen

-- | Problem 25
-- Generate a random permutation of the elements of a list
rnd_permu :: [a] -> IO [a]
rnd_permu xs = map (xs !!) <$> take n . nub . randomRs (0, n-1) <$> getStdGen
  where n = length xs

-- | Problem 26
-- Generate combinations of K distinct objects chosen from the N elements of a list
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs
  | n > length xs = []
  | n == length xs = [xs]
  | otherwise = [x:ys | x:rest <- tails xs,
                        ys <- combinations (n-1) rest]

-- | Problem 27
-- Group the elements of a set into disjoint subsets

-- | Part A
-- How many ways can we group 9 people into 3 disjoint subgroups of 2, 3, and 4 persons
group3 :: Eq a => [a] -> [[[a]]]
group3 = group [2,3,4]

-- | Part B
-- Generalize that into a way we can specify a list of group sizes
group :: Eq a => [Int] -> [a] -> [[[a]]]
group [] [] = [[]]
group [] _  = []
group (n:ns) xs 
  | n < 0 || sum (n:ns) > length xs = []
  | otherwise = 
      [ g : groups 
      | g <- combinations n xs,
        groups <- group ns (filter (`notElem` g) xs) ]

-- | Problem 28
-- Sorting a list of lists according to length of sublists

-- | Part A
-- Sort the elements of the list according to their length
lsort :: [[a]] -> [[a]]
lsort = sortBy (compare `on` length)

-- | Part B
-- Sort the elements of the list according to their length frequency
lfsort :: [[a]] -> [[a]]
lfsort lists = sortBy (compare `on` frequency) lists
  where
    fm = M.fromListWith (+) [(length xs, 1) | xs <- lists]
    frequency = (fm M.!) . length

-- | Problem 31
-- Determine whether a given integer number is prime
isPrime :: Integral a => a -> Bool
isPrime n = n > 1 && null [d | d <- [2..isqrt n], n `mod` d == 0]
  where isqrt = floor . sqrt . fromIntegral

-- | Problem 32
-- Determine the greatest common divisor of two positive integer numbers
myGCD :: Integral a => a -> a -> a
myGCD a 0 = abs a
myGCD a b = myGCD b (a `mod` b)

-- | Problem 33
-- Determine whether two positive integer numbers are coprime
coprime :: Integral a => a -> a -> Bool
coprime x y = myGCD x y == 1

-- | Problem 34
-- Calculate Euler's totient function phi(m)
totient :: Integral a => a -> Int
totient 1 = 1
totient m = length [r | r <- [1..m-1], r `coprime` m]

-- | Problem 35
-- Determine the prime factors of a given positive integer
primeFactors :: Integral a => a -> [a]
primeFactors n
    | n < 2     = []
    | n == 2    = [2]
    | even n    = 2 : primeFactors (n `div` 2)
    | otherwise = case factors of
        [] -> [n]  -- n is prime
        (f:_) -> f : primeFactors (n `div` f)
  where
    isqrt = floor . sqrt . fromIntegral
    divides d n = n `mod` d == 0
    factors = [x | x <- [3,5..isqrt n], divides x n, isPrime x]

-- | Problem 36
-- Determine the prime factors and their multiplicites of a given positive integer
prime_factors_mult :: Integral a => a -> [(a, Int)]
prime_factors_mult = map (\(a, b) -> (b, a)) . encode . primeFactors

-- | Problem 37
-- Calculate Euler's totient function phi(m) (improved)
--
-- For a number m with prime factorization ((p1,m1) (p2,m2) (p3,m3) ...),
-- phi(m) can be efficiently calculated as:
--
-- phi(m) = (p1-1) * p1^(m1-1) * 
--          (p2-1) * p2^(m2-1) * 
--          (p3-1) * p3^(m3-1) * ...
totient' :: Integral a => a -> a
totient' = product . map (\(p,m) -> (p-1) * p^(m-1)) . prime_factors_mult

-- | Problem 38
-- Compare the two methods of calculating Euler's totient functions
-- No solution required

-- | Problem 39
-- A list of prime numbers in a given range
primesR :: Integral a => a -> a -> [a]
primesR lo hi = [x | x <- [lo..hi], isPrime x]

-- | Problem 40
-- Goldbach's conjecture
goldbach :: Integral a => a -> (a, a)
goldbach n
  | n <= 2 || odd n = error "must be even and greater than 2"
  | otherwise       = head [(x, n-x) | x <- primesR 1 (n-1), isPrime (n-x)]

-- | Problem 41
-- A list of even numbers and their Goldbach compositions in a given range
goldbachList :: Integral a => a -> a -> [(a, a)]
goldbachList lo hi =
  [ goldbach n
  | n <- filter even [lo..hi]
  ]

goldbachList' :: Integral a => a -> a -> a -> [(a, a)]
goldbachList' lo hi thresh = filter (both (>thresh)) $ goldbachList lo hi
  where both f (x, y) = (f x) && (f y)
