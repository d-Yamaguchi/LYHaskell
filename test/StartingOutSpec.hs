module StartingOutSpec
    (spec
    ) where

import Test.Hspec
import Test.Hspec.QuickCheck
import StartingOut
import Answer as Ans
import Data.List (sort)
import Data.Function(on)

listeq :: Ord a => [a] -> [a] -> Bool
listeq = (==) `on` sort

spec :: Spec
spec = do
  describe "Prob.2-1 Mamhattan Distance" .
    prop "manlen" $ \p1 p2 -> StartingOut.manlen p1 p2 == Ans.manlen p1 p2

  describe "Prob.2-2 List of Lattice points" .
    prop "points" $ \n -> StartingOut.points n `listeq` Ans.points n
