# LYHaskell


## Directory structure
- `exercises` : contains exercises corresponding to each chapter.
- `src` : contains emplates for solving problems. Please edit this file directly.
- `test` : contains automatic test to comferm whether your answer is correct.

## How to check your answer
```shell
$ stack test

StartingOut
  Prob.2-1 Mamhattan Distance
    manlen ✅Correct answer
  Prob.2-2 List of Lattice points
    points FAILED [1] ⚠️Incorrect answer
```
`points FAILED` means function `points`, which you defined, is incorrect.
So you have to fix it.

After fixing your answer, retest it.
```shell
$ stack test

StartingOut
  Prob.2-1 Mamhattan Distance
    manlen ✅Correct answer
  Prob.2-2 List of Lattice points
    points ✅Correct answer
```
