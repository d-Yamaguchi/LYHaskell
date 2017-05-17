module SyntaxInFunctions    (
      triPattern
    , triGuard
    , triCase
    , qadd
    , qequal
    , qlist
    ) where

    -- prob.4-1
    triPattern :: Int -> Int
    triPattern 0 = 0
    triPattern 1 = 0
    triPattern 2 = 1
    triPattern n = triPattern ( n - 1 ) + triPattern ( n - 2 ) + triPattern ( n - 3 )

    triGuard :: Int -> Int
    triGuard n
        | n <= 1 = 0
        | n == 2 = 1
        | n >= 3 = triGuard ( n - 1 ) + triGuard ( n - 2 ) + triGuard ( n - 3 )

    triCase :: Int -> Int
    triCase n = case n of 0 -> 0
                          1 -> 0
                          2 -> 1
                          n -> triCase ( n - 1 ) + triCase ( n - 2 ) + triCase ( n - 3 )

    -- prob.4-2
    qadd :: (Int, Int) -> (Int, Int) -> (Int, Int)
    qadd (_,0) (_,_) = error "second of tuple number is 0"
    qadd (_,_) (_,0) = error "second of tuple number is 0"
    qadd (a,b) (c,d) = (a*d + b*c, b*d)

    qequal :: (Int, Int) -> (Int, Int) -> Bool
    qequal (_,0) (_,_) = error "second of tuple number is 0"
    qequal (_,_) (_,0) = error "second of tuple number is 0"
    qequal (a,b) (c,d) = a*d == b*c

    qlist :: (Int, Int) -> [(Int, Int)]
    qlist (_,0) = error "second of tuple number is 0"
    qlist (a,b) = [ ( a * n * s `div` gcd a b , b * n * s `div` gcd a b ) | n <- [1..] , s <- [1,-1] ]
