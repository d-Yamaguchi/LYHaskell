# Exercises2. StartingOut
* These prpbrems are correspond to Chapter1 (StartingOut).
* Fill in `src/StartingOut.hs` with your code.

---

## Prob.2-1 Mamhattan Distance
Define a function `manlen` that takes two integer pairs and returns the Manhattan distance of those two points.

The Manhattan distance `manlen(p1, p2)`, where `p1 = (a, b)` and `p2 = (c, d)` is defined by the following equation.

```haskell
manlen(p1, p2) = abs(a - c) + abs(b - d)
```

### Hint

```haskell
manlen :: (Int, Int) -> (Int, Int) -> Int

*Main> abs 1
1
*Main> abs (-1)
1
*Main> manlen (0,0) (1,1)
2
*Main> manlen (-1,1) (1,3)
4
```

---

## Prob.2-2 List of Lattice points
Define functions `points` that accept nonnegative integers and return a list of integer pairs whose absolute values are within that number.

For example, when Integer `1` is given, points returns a set `{(x,y) | x ∈ {-1, 0, 1}, y ∈ {-1, 0, 1}}`

Also, you do not need to consider the behavior when giving a negative number.

### Hint
```haskell
points :: Int -> [(Int, Int)]

*Main> points 0
[(0,0)]
*Main> points 1
[(-1,-1),(-1,0),(-1,1),(0,-1),(0,0),(0,1),(1,-1),(1,0),(1,1)]
```
