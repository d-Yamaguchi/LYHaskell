# Exercises4. SyntaxInFunctions
* These prpbrems are correspond to Chapter3 (SyntaxAndTypeClasses).
* Fill in `src/SyntaxInFunctions.hs` with your code.

---

## Prob.4-1 Tribonacci Sequence
Define the function that calculates the n-th number of Tribonacci sequence with the following names and methods.
  1. `tri_pattern` : with pattern match
  2. `tri_guard` : with guard
  3. `tri_case` : with case expression

Note that the first number shall be the 0th.
Also, you do not need to consider the behavior when giving a negative number.

### Definintion 4.1 Tribonacci Sequence
The tribonacci numbers are a generalization of the Fibonacci numbers defined by $T_0=0, T_1=0, T_2=1$, and the recurrence equation $T_n=T_(n-1)+T_(n-2)+T_(n-3)$
for $n>=3$.

The first 10 terms are as follows.
```
0, 0, 1, 1, 2, 4, 7, 13, 24, 44
```

### Hint
```haskell
*Main> :t tri_pattern
tri_pattern :: Int -> Int
*Main> take 10 [tri_pattern n | n <- [0..]]
[0,0,1,1,2,4,7,13,24,44]

tri_guard :: Int -> Int
*Main> take 10 [tri_guard n | n <- [0..]]
[0,0,1,1,2,4,7,13,24,44]

*Main> :t tri_case
tri_case :: Int -> Int
*Main> take 10 [tri_case n | n <- [0..]]
[0,0,1,1,2,4,7,13,24,44]
```

## Prob.4-2 Tupled Number
Define the following function. Note that it is not necessary to consider the behavior when giving an extremely large number, such as overflowing.
1. `qadd` : A function that accepts two tupled numbers and returns the sum (it does not have to be irreducible)
2. `qequal` : A function that takes two tupled numbers and returns a boolean value as to whether the values as rational numbers are equal
3. `qlist` : A function that takes one tupled number and returns a list of all tuples equal to that tuple number. However, the order shall be in descending order of the absolute value of denominator, and if the absolute values are the same, the one with the positive denominator will be the first.  

### Definintion 4.2 Tupled Number
Let rational numbers be represented by two pairs of numerator and denominator representing integers.
We call the number according to this expression a tupled number.
For instance, 2/5 can be represented by the tupled number like `(2,5)`.

The points to be noted are listed below.
- Both the numerator and denominator allow to be negative numbers. 2/5 can also be expressed as `(2,5)` or `(-2, -5)`.
- Allow expressions that are not irreducible. 2/5 can also be expressed as `(4, 10)` or `(20, 50)`.
- There is no tupled number whose denominator is `0`. When a function is applied to such a number of tuples, it is regarded as a runtime exception.

### Hint
```haskell
*Main> :t qadd
qadd :: (Int, Int) -> (Int, Int) -> (Int, Int)
*Main> qadd (1,2) (3,4)
(10,8)
*Main> qadd (1,1) (-2,-5)
(-7,-5)
*Main> qadd (1,1) (-2,0)
*** Exception: second of tuple number is 0

*Main> :t qequal
qequal :: (Int, Int) -> (Int, Int) -> Bool
*Main> qequal (1,2) (-2,-4)
True
*Main> qequal (1,3) (-2,-4)
False
*Main> qequal (1,0) (-2,-4)
*** Exception: second of tuple number is 0

*Main> :t qlist
qlist :: (Int, Int) -> [(Int, Int)]
*Main> take 10 (qlist (4,6))
[(2,3),(-2,-3),(4,6),(-4,-6),(6,9),(-6,-9),(8,12),(-8,-12),(10,15),(-10,-15)]
*Main> take 10 (qlist (3,-5))
[(-3,5),(3,-5),(-6,10),(6,-10),(-9,15),(9,-15),(-12,20),(12,-20),(-15,25),(15,-25)]
*Main> take 10 (qlist (1,0))
*** Exception: second of tuple number is 0
```
