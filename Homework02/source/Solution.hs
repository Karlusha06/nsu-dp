module Solution
    ( unique
    , pythagoreanTriples
    , primitivePythagoreanTriples
    , perfectNumbers
    , cantorPairs
    , minimalDistance
    ) where
----task1
check :: Eq a => [a] -> a -> Bool
check [] _ = True
check l el 
    | (head l) == el = False
    | otherwise = check (tail l) el

unique :: Eq a => [a] -> Bool
unique [] = True
unique l
    | (check (tail l) (head l)) == True = unique (tail l)
    | otherwise = False

----task2
pythagoreanTriples :: Integral a => [(a, a, a)]
pythagoreanTriples = [(x, y, z) | z <- [1..], y <- [1..z], x <- [1..y], x^2 + y^2 == z^2]

----task3
primitivePythagoreanTriples :: Integral a => [(a, a, a)]
primitivePythagoreanTriples = [(a, b, c) | m <- [1..], n <- [1..m-1], let a = m^2 - n^2, let b = 2*m*n, let c = m^2 + n^2, gcd m n == 1, a^2 + b^2 == c^2]

----task4
perfectNumbers :: Integral a => [a]
perfectNumbers = [x | x <- [1..], sum (properDivisors x) == x] where
    properDivisors n = [i | i <- [1..n-1], n `mod` i == 0]

----task5
cantorPairs :: Integral a => [(a, a)]
cantorPairs = undefined

----task6
minimalDistance :: RealFloat a => [(a, a)] -> a
minimalDistance [] = 1 / 0
minimalDistance [_] = 1 / 0
minimalDistance xs = minDistance' xs (1/0)
  where
    minDistance' [] _ = 1/0
    minDistance' [_] minDist = minDist
    minDistance' (p:ps) minDist = let newMinDist = foldl (\acc x -> min acc (distance p x)) minDist ps
                                  in minDistance' ps newMinDist
    distance (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

