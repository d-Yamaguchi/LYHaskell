module SyntaxInFunctionsSpec
    (spec
    ) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Data.Function
import Data.Functor
import Control.Exception hiding (assert)
import SyntaxInFunctions
import Answer as Ans

tryEval' :: a -> IO (Either SomeException a)
tryEval' = try . evaluate

tryEval :: a -> IO (Maybe a)
tryEval = fmap eitherToMaybe . tryEval'

qnorm :: (Int, Int) -> (Int, Int)
qnorm (a,0) = (a,0)
qnorm (a,b) | b < 0 = qnorm (-a,-b)
qnorm (a,b) = (a `div` g, b `div` g)
  where g = gcd a b

qeq :: Maybe (Int, Int) -> Maybe (Int, Int) -> Bool
qeq = (==) `on` fmap qnorm

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right x) = Just x
eitherToMaybe (Left _) = Nothing

---Test spec

spec :: Spec
spec = do
  describe "Prob.4-1 Tribonacci Sequence" $ do
    prop "tri_pattern" $ \n -> SyntaxInFunctions.triPattern (mod n 20) == Ans.triPattern (mod n 20)
    prop "tri_guard" $ \n -> SyntaxInFunctions.triGuard (mod n 20) == Ans.triGuard (mod n 20)
    prop "tri_case" $ \n -> SyntaxInFunctions.triCase (mod n 20) == Ans.triCase (mod n 20)

  describe "Prob.4-2 Tupled Number" $ do
    prop "qadd" $ \q1 q2 ->  SyntaxInFunctions.qadd q1 q2 == Ans.qadd q1 q2
    prop "qequal" $ \q1 q2 -> SyntaxInFunctions.qequal q1 q2 == Ans.qequal q1 q2
    prop "qlist" $ \q -> SyntaxInFunctions.qlist q == Ans.qlist q
