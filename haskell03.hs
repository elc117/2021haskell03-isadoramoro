-- Prática 03 de Haskell
-- Nome: Isadora Silveira Moro

-- 1)
add10total :: [Int] -> [Int]
add10total lis = [x+10 | x <- lis]

-- 2)
multN :: Int -> [Int] -> [Int]
multN n lis = [x*n | x <- lis]

-- 3)
multN' :: Int -> [Int] -> [Int]
multN' n lis = map (\x -> x*n) lis

-- 4)
applyExpr :: [Int] -> [Int]
applyExpr lis = [3*x+2 | x <- lis]

-- 5)
applyExpr' :: [Int] -> [Int]
applyExpr' lis = map (\x -> 3*x+2) lis

-- 6)
addSuffix :: String -> [String] -> [String]
addSuffix suf lis = [x ++ suf | x <- lis]

-- 7)
selectgt5 :: [Int] -> [Int]
selectgt5 lis = [x | x <- lis, x > 5]

-- 8)
sumOdds :: [Int] -> Int
sumOdds lis = foldl (+) 0 [x | x <- lis, odd x]

-- 9)
sumOdds' :: [Int] -> Int
sumOdds' lis = foldl (+) 0 (filter odd lis)

-- 10)
selectExpr :: [Int] -> [Int]
selectExpr lis = [x | x <- lis, x >= 20 && x <= 50 && even x]

-- 11)
countShorts :: [String] -> Int
countShorts lis = length [x | x <- lis, length x < 5]

-- 12)
calcExpr :: [Float] -> [Float]
calcExpr lis = filter (> 10) [x^2/2 | x <- lis]

-- 13)
--trSpaces :: String -> String

-- 14)
--selectSnd :: [(Int,Int)] -> [Int]

-- 15)
--dotProd :: [Int] -> [Int] -> Int
