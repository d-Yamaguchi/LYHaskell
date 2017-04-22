module StartingOut (manlen, points) where

-- Mamhattan Distance
-- Define a function 'manlen' that takes two integer pairs and returns the Manhattan distance of those two points.
-- The Manhattan distance 'manlen(p1, p2)' where p1 = (a, b) and p2 = (c, d) is defined by the following equation.
-- manlen(p1, p2) = abs(a - c) + abs(b - d)
manlen :: (Int, Int) -> (Int, Int) -> Int
manlen p q = abs (fst p - fst q) + abs (snd p - snd q)

-- List of Lattice points
-- Define functions points that accept nonnegative integers and return a list of integer pairs whose absolute values are within that number.
-- Forexample, When Integer 1 is given, points returns [(x,y) | x in [-1,0,-1], y in [-1, 0, 1]]
points :: Int -> [(Int, Int)]
points n = [(p,q) | p <- [-n..n], q <- [-n..n]]
