module Main where

main :: IO ()
main = putStrLn "use ghci"


-- My Memo ---

{-Lecture1-}
factorial 0 = 1
factorial x = x * factorial (x-1)

{-Nat -}
data Nat = Zero
         | Succ Nat
   deriving(Ord,Eq,Show)

(＋)  :: Nat -> Nat -> Nat
m ＋ Zero    = m
m ＋ Succ n  = Succ ( m ＋ n )

(×)  :: Nat -> Nat -> Nat
m × Zero    = Zero
m × Succ n  = (m × n) ＋ m

factNat :: Nat -> Nat
factNat Zero = Succ Zero
factNat (Succ n) = Succ n × factNat n