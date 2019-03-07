-- Solutions to the P-99 problems in Haskell
import Data.List
import System.Random
--P01
myLast :: [a] -> a
myLast []     = error "empty list"
myLast [x]    = x
myLast (_:xs) = myLast xs

--P02
penultimate :: [a] -> a
penultimate []     = error "empty list"
penultimate [_]    = error "list with only one item"
penultimate [a, b] = a
penultimate (_:xs) = penultimate xs

--P03
elemAt :: Int -> [a] -> a
elemAt _ []     = error "index out of bounds"
elemAt 1 (x:_)  = x
elemAt i (_:xs)
    | i < 1     = error "index out of bounds"
    | otherwise = elemAt (i - 1) xs

--P04
myLength :: [a] -> Int
myLength = foldl (\n _ -> n + 1) 0

--P05
myReverse :: [a] -> [a]
myReverse = foldl (\a x -> x:a) []

--P06
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == (reverse xs)

--P07
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x)  = [x]
flatten (List xs) = xs >>= flatten

--P08
keepIfDifferent :: (Eq a) => a -> [a] -> [a] 
keepIfDifferent x z
    | z == []     = [x]
    | x == head z = z
    | otherwise = x : z

compress :: (Eq a) => [a] -> [a]
compress = foldr keepIfDifferent []

compressBuiltIns :: Eq a => [a] -> [a]
compressBuiltIns = map head . group

--P09
groupIfSame :: (Eq a) => a -> [[a]] -> [[a]]
groupIfSame x z
    | z == []         = [[x]]
    | x == (head $ head z) = (x : head z) : tail z
    | otherwise            = [x] : z

pack :: (Eq a) => [a] -> [[a]]
pack = foldr groupIfSame []

packBuiltIn :: Eq a => [a] -> [[a]]
packBuiltIn = group

packGroupImpl :: Eq a => [a] -> [[a]]
packGroupImpl (x:xs) = let (group, rest) = span (==x) xs
                        in (x:group) : packGroupImpl rest
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
        expandItems (Single x)     = [x]
        expandItems (Multiple n x) = replicate n x

--P13 
encode3 :: Eq a => [a] -> [ListItem a]
encode3 []     = []
encode3 (x:xs) = let (group, rest) = span (==x) xs in 
    convertIfSingle (Multiple (1 + length group) x) : encode3 rest
    where convertIfSingle (Multiple 1 x) = (Single x)
          otherwise                      = x

--P14
duplicate :: [a] -> [a]
duplicate = concatMap $ replicate 2

--P15
duplicateN :: Int -> [a] -> [a]
duplicateN n = concatMap $ replicate n

--P16
dropEveryNth :: Int -> [a] -> [a]
dropEveryNth n xs = [x | (i, x) <- zip [0..] xs, (i + 1) `mod` n /= 0] 

--P17
splitAt2 :: Int -> [a] -> ([a], [a])
splitAt2 n xs = (take n xs, drop n xs)

--Do recursive as well

--P18
slice :: Eq a => Int -> Int -> [a] -> [a]
slice start end = take (end - max start 0) . drop start 

--P19
rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = let (left, right) = splitAt ((length xs + n) `mod` length xs) xs
    in right ++ left

--P20 
removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = let (start, (y:ys)) = splitAt n xs in (y, start ++ ys)

--P21
insertAt :: Int -> [a] -> a -> [a]
insertAt n xs e = let (start, end) = splitAt n xs in start ++ (e : end)

--P22
range :: Int -> Int -> [Int]
range a b = [a..b]

--P23
rnd_select xs n = do
    gen <- newStdGen
    return $ take n [ xs !! x | x <- randomRs (0, (length xs) - 1) gen]

--P24
