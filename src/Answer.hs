module Answer where

import Data.Ord
import Data.List hiding (insert)
import qualified Data.Map as Map
import Data.Char
import Data.Bits

--Chapter2 StartingOut
-- prpb.2-1
manlen :: (Int, Int) -> (Int, Int) -> Int
manlen p1 p2 = abs(fst p1 - fst p2) + abs(snd p1 - snd p2)

-- prob.2-2
points :: Int -> [(Int, Int)]
points n = [(x,y) | x <- [-n .. n], y <- [-n .. n]]

--Chapter4 SyntaxInFunctions
-- prob.4-1(a)
triPattern :: Int -> Int
triPattern 0 = 0
triPattern 1 = 0
triPattern 2 = 1
triPattern n = triPattern (n-1) + triPattern (n-2) + triPattern (n-3)

-- prob.4-1(b)
triGuard :: Int -> Int
triGuard n
    | n == 0 = 0
    | n == 1 = 0
    | n == 2 = 1
    | otherwise = triGuard (n-1) + triGuard (n-2) + triGuard (n-3)

-- prob.4-1(c)
triCase :: Int -> Int
triCase n = case n of
               0 -> 0
               1 -> 0
               2 -> 1
               _ -> triCase (n-1) + triCase (n-2) + triCase (n-3)

-- prob.4-2(a)
qadd :: (Int, Int) -> (Int, Int) -> (Int, Int)
qadd (_,0) (_,_) = error "second of tuple number is 0"
qadd (_,_) (_,0) = error "second of tuple number is 0"
qadd (a,b) (c,d) = (a * d + b * c, b * d)

-- prob.4-2(b)
qequal :: (Int, Int) -> (Int, Int) -> Bool
qequal (_,0) (_,_) = error "second of tuple number is 0"
qequal (_,_) (_,0) = error "second of tuple number is 0"
qequal (a,b) (c,d) = a * d == b * c

-- prob.4-2(c)
qlist :: (Int, Int) -> [(Int, Int)]
qlist (_,0) = error "second of tuple number is 0"
qlist (a,b) | b < 0 = qlist (-a,-b)
qlist (a,b) =
    let g = gcd a b
        a' = a `div` g
        b' = b `div` g
    in [(s * a' * n, s * b' * n) | n <- [1..], s <- [1,-1]]


--Chapter5 Recursion
-- prob.5-1(a)
triNumber :: Int -> Int
triNumber 0 = 0
triNumber n = n + triNumber (n-1)

-- prob.5-1(b)
tetration :: Integer -> Integer -> Integer
tetration _ 0 = 1
tetration x n = x^tetration x (n-1)

-- prob.5-1(c)
index :: Int -> [a] -> a
index _ [] = error "index outbounded"
index 0 (x:_) = x
index n (_:xs) = index (n-1) xs

-- prob.5-1(d)
evenOdd :: [Int] -> ([Int], [Int])
evenOdd [] = ([], [])
evenOdd (x:xs)
    | x `mod` 2 == 0 = (odds, x:evens)
    | otherwise  = (x:odds, evens)
    where (odds, evens) = evenOdd xs

-- prob.5-2(a)
insert :: Ord a => [a] -> a -> [a]
insert [] y = [y]
insert xs@(x:_) y | y < x = y:xs
insert (x:xs) y = x:insert xs y

-- prob.5-2(b)
isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert (isort xs) x

--- prob.5-3
partNum :: Int -> Int
partNum n = partNum' n 1
    where partNum' n m
           | n == m = 1
           | n < m  = 0
           | otherwise = partNum' (n-m) m + partNum' n (m+1)

--Chapter6 HigherOederFunctions
-- prob.6-1(a)
applyPair :: (a -> b) -> (a, a) -> (b, b)
applyPair f (x,y) = (f x, f y)

-- prob.6-1(b)
applyN :: (a -> a) -> Int -> a -> a
applyN _ 0 = id
applyN f n = (f.) $ applyN f $ n - 1

-- prob.6-1(c)
squares :: Int -> [Int]
squares n = takeWhile (<= n) $ map (^2) [1..]

-- prob.6-2(a)
fromBinary :: [Int] -> Int
fromBinary = foldl ((+) . (*2)) 0

-- prob.6-2(b)
tails :: [a] -> [[a]]
tails = scanr (:) []

-- prob.6-2(c)
powerSet :: [a] -> [[a]]
powerSet = foldr (\x acc -> map (x:) acc ++ acc) [[]]

-- prob.6-3(a)
pointed1 :: [Int] -> [Int]
pointed1 xs = map negate (map (+10) (filter (>0) xs))

pointFree1 :: [Int] -> [Int]
pointFree1 = map (negate . (+10)) . filter (>0)

-- prob.6-3(b)
pointed2 :: [[Int]] -> [Int]
pointed2 xss = scanl (+) 0 (map (foldl (*) 1) (filter (\xs -> length xs >= 2) xss))

pointFree2 :: [[Int]] -> [Int]
pointFree2 = scanl (+) 0 . map (foldl (*) 1) . filter ((>=2) . length)

-- prob.6-3(c)
pointed3 :: [a -> a] -> a -> a
pointed3 fs x = foldl (\x f -> f x) x fs

pointFree3 :: [a -> a] -> a -> a
pointFree3 = flip $ foldl $ flip ($)

-- prob.6-3(d)
pointed4 :: (a -> [b]) -> [a] -> [b]
pointed4 f xs = concat (map f xs)

pointFree4 :: (a -> [b]) -> [a] -> [b]
pointFree4 = (concat.) . map

-- prob.6-3(e)
pointed5 :: (Int -> [Int]) -> [Int] -> [Int]
pointed5 f xs = foldl (\ys g -> g ys) xs (replicate 3 (\zs -> concat (map f zs)))

pointFree5 :: (Int -> [Int]) -> [Int] -> [Int]
pointFree5 = (flip $ foldl $ flip ($)) . replicate 3 . ((concat.) . map)

-- prob.6-4(a)
church 0 _ z = z
church n f z = f $ church (n-1) f z

unchurch c = c (+1) 0

csucc c f z = f $ c f z

cadd c1 c2 f z = c1 f (c2 f z)

cmul c1 c2 f z = c1 (c2 f) z

cpred c f z = c (\g h -> h (g f)) (\u -> z) (\u -> u)

-- prob.6-4(b)
--(4.2 pre-defined)
cTrue :: t -> t -> t
cTrue = \t f -> t

cFalse :: t -> t -> t
cFalse = \t f -> f

churchb True = cTrue
churchb False = cFalse

unchurchb cb = cb True False

cnot cb = cb cFalse cTrue

cand cb1 cb2 = cb1 cb2 cFalse

cor cb1 cb2 = cb1 cTrue cb2

--prob.6-4(c)
-- (4.3 pre-defined)
cif :: (Bool -> Bool -> Bool) -> t -> t -> t
cif cb t f = if unchurchb cb then t else f

cand' cb1 cb2 = cif cb1 cb2 cFalse
cis0 c = c (cand' cFalse) cTrue

cnot' cb = cif cb cFalse cTrue
ceven c = c cnot' cTrue

cevennot0 c = ceven c `cand'` (cnot' $ cis0 c)

clte2 c = cis0 $ cpred $ cpred c


--Chapter7 Modules
-- prob.7-1(a)
sortByFrequency :: Ord a => [a] -> [a]
sortByFrequency = map head . sortBy (comparing $ Down . length) . group . sort

-- prob.7-1(b)
initialMap :: [String] -> Map.Map Char [String]
initialMap = Map.fromListWith (++) . map (\s -> (head s, [s])) . filter (not . null)

-- prob.7-1(c)
infixPalindromicNumber :: Int -> Int
infixPalindromicNumber n = head [m | m <- [0..], let m' = show m, reverse m' == m', show n `isInfixOf` m']

-- prob.7-2
vernam :: String -> String -> String
vernam k s = map chr $ zipWith xor (map ord $ cycle k) (map ord s)
