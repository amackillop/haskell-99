module Problems where

import qualified Data.List                     as List
import qualified System.Random                 as Random
import qualified Data.Function                 as Function

-- Solutions to the P-99 problems in Haskell
--P01
myLast :: [a] -> a
myLast []       = error "empty list"
myLast [x     ] = x
myLast (_ : xs) = myLast xs

--P02
penultimate :: [a] -> a
penultimate []       = error "empty list"
penultimate [_]      = error "list with only one item"
penultimate [a, _]   = a
penultimate (_ : xs) = penultimate xs

--P03
elemAt :: Int -> [a] -> a
elemAt _ []      = error "index out of bounds"
elemAt 1 (x : _) = x
elemAt i (_ : xs) | i < 1     = error "index out of bounds"
                  | otherwise = elemAt (i - 1) xs

--P04
myLength :: [a] -> Int
myLength = foldl (\n _ -> n + 1) 0

--P05
myReverse :: [a] -> [a]
myReverse = foldl (\a x -> x : a) []

--P06
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == (reverse xs)

--P07
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x ) = [x]
flatten (List xs) = xs >>= flatten

--P08
keepIfDifferent :: (Eq a) => a -> [a] -> [a]
keepIfDifferent x z | z == []     = [x]
                    | x == head z = z
                    | otherwise   = x : z

compress :: (Eq a) => [a] -> [a]
compress = foldr keepIfDifferent []

compressBuiltIns :: Eq a => [a] -> [a]
compressBuiltIns = map head . List.group

--P09
groupIfSame :: (Eq a) => a -> [[a]] -> [[a]]
groupIfSame x z | null z             = [[x]]
                | x == head (head z) = (x : head z) : tail z
                | otherwise          = [x] : z

pack :: (Eq a) => [a] -> [[a]]
pack = foldr groupIfSame []

packBuiltIn :: Eq a => [a] -> [[a]]
packBuiltIn = List.group

packGroupImpl :: Eq a => [a] -> [[a]]
packGroupImpl (x : xs) =
  let (group, rest) = span (== x) xs in (x : group) : packGroupImpl rest
packGroupImpl [] = []

--P10
encode :: Eq a => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . pack

--P11
data ListItem a = Single a | Multiple Int a deriving (Show)
encode2 :: Eq a => [a] -> [ListItem a]
encode2 = map reduceSingles . encode
 where
  reduceSingles (1, x) = Single x
  reduceSingles (n, x) = Multiple n x

--P12
decode2 :: [ListItem a] -> [a]
decode2 = concatMap expandItems
 where
  expandItems (Single x    ) = [x]
  expandItems (Multiple n x) = replicate n x

--P13 
encode3 :: Eq a => [a] -> [ListItem a]
encode3 [] = []
encode3 (x : xs) =
  let (group, rest) = span (== x) xs
  in  convertIfSingle (Multiple (1 + length group) x) : encode3 rest
 where
  convertIfSingle (Multiple 1 x) = Single x
  convertIfSingle x              = x

--P14
duplicate :: [a] -> [a]
duplicate = concatMap $ replicate 2

--P15
duplicateN :: Int -> [a] -> [a]
duplicateN n = concatMap $ replicate n

--P16
dropEveryNth :: Int -> [a] -> [a]
dropEveryNth n xs = [ x | (i, x) <- zip [0 ..] xs, (i + 1) `mod` n /= 0 ]

--P17
splitAt2 :: Int -> [a] -> ([a], [a])
splitAt2 n xs = (take n xs, drop n xs)

--Using recursion
splitAt3 :: Int -> [a] -> ([a], [a])
splitAt3 n xs = if n < 0 then ([], xs) else splitR n xs []
 where
  splitR 0 xs       accum = (reverse accum, xs)
  splitR _ []       accum = (reverse accum, [])
  splitR n (x : xs) accum = splitR (n - 1) xs (x : accum)

--P18
slice :: Eq a => Int -> Int -> [a] -> [a]
slice start end = take (end - max start 0) . drop start

--P19
rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs =
  let (left, right) = splitAt ((length xs + n) `mod` length xs) xs
  in  right ++ left

--P20 
removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = let (start, (y : ys)) = splitAt n xs in (y, start ++ ys)

--P21
insertAt :: Int -> [a] -> a -> [a]
insertAt n xs e = let (start, end) = splitAt n xs in start ++ (e : end)

--P22
range :: Int -> Int -> [Int]
range a b = [a .. b]

--P23
-- Note: The use of nub is simple but inefficient. Especially for permuting 
--       large lists.
randomIntGen :: Int -> Int -> IO [Int]
randomIntGen lo hi = Random.randomRs (lo, hi) <$> Random.newStdGen

randSelect :: Int -> [a] -> IO [a]
randSelect n xs = do
  i <- indices
  return $ (xs !!) <$> i
 where
  indices = take max' . List.nub <$> randomIntGen 0 (length xs - 1)
  max'    = min n (length xs)

--P24
lotto :: Int -> Int -> IO [Int]
lotto n m = randSelect n [1 .. m]

--P25
permute :: Eq a => [a] -> IO [a]
permute xs = let n = length xs in randSelect n xs

--P26
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations size xs =
  [ xs !! i : x
  | i <- [0 .. length xs - 1]
  , x <- combinations (size - 1) (drop (i + 1) xs)
  ]

--P27
--ignore for now

--P28
lenSort :: [[a]] -> [[a]]
lenSort = List.sortOn length

lenFreqSort :: [[a]] -> [[a]]
lenFreqSort =
  concat . lenSort . List.groupBy ((==) `Function.on` length) . lenSort

--P31
isFactorOf :: Integral a => a -> a -> Bool
isFactorOf x y = x `rem` y == 0

intSqrt :: Integral a => a -> a
intSqrt = floor . sqrt . fromIntegral

factors :: Integral a => a -> [a]
factors num | num < 1   = []
            | num == 1  = [1]
            | otherwise = 1 : num : concat factors'
 where
  factors' =
    [ if n ^ 2 /= num then [n, num `div` n] else [n]
    | n <- filter (isFactorOf num) [2 .. intSqrt num]
    ]

isPrime :: Integral a => a -> Bool
isPrime n = case factors n of
  [1, _] -> True
  _      -> False


--P32
gcd' :: Integral a => a -> a -> a
gcd' a b | a == 0    = b'
         | b == 0    = a'
         | b > a     = gcd' b' a'
         | otherwise = gcd' b' (a' `rem` b')
 where
  a' = abs a
  b' = abs b

--P33
coPrime :: Integral a => a -> a -> Bool
coPrime a b = gcd' a b == 1

--P34
totient :: Int -> Int
totient a = length $ filter (coPrime a) [1..a-1]

--P35
primeFactors :: Integral a => a -> [a]
primeFactors = filter isPrime . factors

--P36