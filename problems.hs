import Data.List (delete, nub, find, maximumBy, permutations, sort, subsequences)

import Text.Read
import Data.Maybe

import Data.Map ()
import qualified Data.Map as Map

import Data.Set ()
import qualified Data.Set as Set

-- GENERAL FUNCTIONS

-- Calculates prime factors of a number
primeFactors :: Int -> [Int]
primeFactors 1 = []
primeFactors n
    | d == [] = [n]
    | otherwise = d ++ primeFactors ( n `div` (head d) )
    where d = take 1 [ x | x <- [2..sqrtA n], n `mod` x == 0 ]

-- Calculates whether a number is prime
-- Checks it isn't equal to 1 and is not divisible by any prime smaller than the square root of it. (More efficient method)
isPrime :: Int -> Bool
isPrime n = n /= 1 && (null $ [ x | x <- (takeWhile (<=sqrtA n) primes), n `mod` x == 0])

-- Slower way of calculating whether a number is prime.
isPrime' :: Int -> Bool 
isPrime' n
    | n < 2 = False 
    | otherwise = isPrime n

-- Square roots an integer and returns the floored result
sqrtA :: Int -> Int 
sqrtA n = (floor :: Double -> Int) $ sqrt $ fromIntegral n

-- Calculates primes
-- First adds 2 then adds each number x which is not divisible by any prime smaller than the square root of x.
primes :: [Int]
primes = 2 : [ x | x <- [3..], all (\y -> x `mod` y /= 0) $ takeWhile (<= sqrtA x) primes] 

-- Converts an integer to array (e.g. convertIntToArray2 35 = [3, 5])
-- More inefficient method of doing so using modulo division
convertIntToArray2 :: Int -> [Int] 
convertIntToArray2 n 
    | n == 0 = []
    | n < 10 = [n] 
    | otherwise = convertIntToArray2 ((n - d) `div` 10) ++ [d]
    where d = n `mod` 10

-- Uses 
convertIntToArray :: Int -> [Int]
convertIntToArray n = catMaybes $ map ( readMaybe . (: "")) (show n)

convertIntegerToArray :: Integer -> [Integer]
convertIntegerToArray n = catMaybes $ map ( readMaybe . (: "")) (show n)

unionA :: [Int] -> [Int] -> [Int]
unionA xs ys = concatMap (\(a, count) -> replicate count a) maxDups
    where 
    countOccurences as = Map.fromListWith (+) (map (\y -> (y, 1)) as)
    xsDups = countOccurences xs
    ysDups = countOccurences ys
    allKeys = Map.keys (Map.union xsDups ysDups)
    maxDups = [(k, max (Map.findWithDefault 0 k xsDups) (Map.findWithDefault 0 k ysDups)) | k <- allKeys] 

factors :: Int -> [Int]
factors n = concatMap (\x -> if (n `div` x) == x then [x] else [x, n `div` x]) [x | x <- [1..sqrtA n], n `mod` x == 0]

properFactors :: Int -> [Int]
properFactors n = 1 : concatMap (\x -> if (n `div` x) == x then [x] else [x, n `div` x]) [x | x <- [2..sqrtA n], n `mod` x == 0]

factors2 :: Int -> [Int]
factors2 n = drop 1 $ factors n 

noFactors :: Int -> Int 
noFactors = length . factors

noFactors2 :: Int -> Int 
noFactors2 = length . factors2

-- PROBLEM 1
sum3n5 :: Int
sum3n5 = sum $ [3,6..999] `union` [5,10..999]
    where union xs ys = xs ++ foldl (flip delete) (nub ys) xs 

-- PROBLEM 2
evenFibSum :: Int 
evenFibSum = sum evenFibs
    where evenFibs = takeWhile (<4000000) [ x | x <- fibs, even x]
          fibs = 0 : 1 : zipWith (+) fibs (tail fibs) 

-- PROBLEM 3
lpf :: Int -> Int 
lpf n = maximum $ primeFactors n

-- PROBLEM 4
createPalindromes :: [Int]
createPalindromes = [ convertArrayToInt $ x ++ reverse x| x <- map convertIntToArray $ reverse [111..999]]

checkPalindromes :: Maybe Int
checkPalindromes = find f createPalindromes
    where f y = not $ null [ x | x <- [100..999], y `mod` x == 0, y `div` x >= 100, y `div` x < 1000]

checkPalindrome :: Int -> [Int]
checkPalindrome y = [ x | x <- [100..999], y `mod` x == 0, y `div` x >= 100, y `div` x < 1000]

-- PROBLEM 5
smallestMultiple :: Int -> Int 
smallestMultiple n = product $ foldr unionA [] (map primeFactors [1..n])

-- PROBLEM 6
sumSquareDif :: Int -> Int 
sumSquareDif n = sumPowDif 2 n

sumPowDif :: Int -> Int -> Int 
sumPowDif p n = sum [1..n] ^ p - sum [ x^p | x <- [1..n] ]

-- PROBLEM 7
primeNumber :: Int -> Int 
primeNumber n = primes !! (n-1)

-- PROBLEM 8
parseNum :: Parser String
parseNum = do 
    xs <- takeWhileP Nothing (/= '\n')
    pure xs

runProgram :: String -> IO () 
runProgram str = do 
    putStrLn (show $ largestProduct2 13 (read str :: Integer))

products :: Int -> Integer -> [Integer]
products d n = helper $ convertIntegerToArray n 
    where 
        helper [] = []
        helper xs = (product $ take d xs) : (helper $ drop 1 xs)

largestProduct :: Integer -> Integer 
largestProduct n = helper $ convertIntegerToArray n
    where 
    helper (x1:x2:x3:x4:xs) = max (x1*x2*x3*x4) (helper (x2:x3:x4:xs))
    helper _ = 1

largestProduct2 :: Int -> Integer -> Integer 
largestProduct2 d n = helper $ convertIntegerToArray n 
    where 
        helper [] = 0
        helper xs = max (product $ take d xs) (helper $ drop 1 xs)

-- PROBLEM 9 :: TODO
-- pythagTriplets :: Maybe (Int, Int, Int)
-- pythagTriplets = find (\(a, b, c) -> a + b + c == 1000) [(m^2 - n^2, 2*m*n, m^2+n^2) | m <- [1..100], n <- [1..100], m^2 - n^2 >= 0]

-- make all pythagTriplets with a, b, c < 1000.

-- PROBLEM 10
sumPrimes :: Int -> Int 
sumPrimes n = sum (takeWhile (<n) primes)

-- PROBLEM 11 :: TODO 
-- i am going to sort the grid into vertical and diagonal strips then use function
-- defined above !

-- PROBLEM 12
triangleNumbers :: [Int]
triangleNumbers = [ triangleNumber x | x <- [1..] ]

triangleNumber :: Int -> Int 
triangleNumber n = sum [1..n] 

-- hdTriangle' :: Int -> Maybe Int 
-- hdTriangle' f = find (\x -> (length $ factors x) > f) triangleNumbers

-- triangleNumberFactors :: Int -> Int 
-- triangleNumberFactors n
--     | even n = 1 + noFactors2 (n `div` 2) + noFactors2 (n + 1) + noFactors2 (n `div` 2) * noFactors2 (n + 1)
--     | otherwise = 1 + noFactors2 n + noFactors2 ( (n+1) `div` 2 ) + noFactors2 n * noFactors2 ( (n+1) `div` 2 )

hdTriangle :: Int -> Int 
hdTriangle lim = helper 1 0
    where
    helper :: Int -> Int -> Int
    helper n a = if numFact > lim then triangleNumber n else helper (n+1) b
        where 
        b
            | even n = noFactors2 $ n+1
            | otherwise = noFactors2 $ (n+1) `div` 2
        numFact = 1 + a + b + a * b

-- PROBLEM 13 :: TODO

-- PROBLEM 14
collatzSeq :: Int -> [Int]
collatzSeq 1 = [1]
collatzSeq n = n : collatzSeq (if even n then n `div` 2 else 3*n + 1)

lengthCS :: Int -> Int 
lengthCS = length . collatzSeq

largestCollatzSeq :: Int -> Int 
largestCollatzSeq n = maximumBy (\x y -> lengthCS x `compare` lengthCS y) (reverse [1..n]) 

fib :: Integer -> Integer 
fib 1 = 1
fib 2 = 1
fib n = fib (n - 2) + fib (n - 1) 

thousFib :: Integer 
thousFib = helper 1
    where 
    helper n = if fib' n >= ((10::Integer)^(999::Integer)) then n else helper $ n+1

fib' :: Integer -> Integer 
fib' n = (floor :: Double -> Integer) $ (1.0 / sqrt 5) * (((1 + sqrt 5) / 2)^n - ((1 - sqrt 5)/2)^n)
-- TODO : look at fast doubling / matrix ways of calculating fib ??

powDigSumA :: Int -> Int 
powDigSumA n = sum $ convertIntToArray2 $ 2^n 

powDigSums :: [Int]
powDigSums = map powDigSumA [1..50]

-- powsOfTwo :: [Int] 
-- powsOfTwo = map (2^) [1..50]

-- secondDigit :: [Int]
-- secondDigit = map ((!!1) . reverse . convertIntToArray . (2^)) [4..50] 

anyDigit :: Int -> [Int]
anyDigit n = map (indexElse . reverse . convertIntToArray . ((2^) :: Int -> Int)) [1..50]
    where 
        indexElse :: [Int] -> Int
        indexElse xs = if length xs >= n then xs!!(n-1) else 0 

-- ah :: FixedPoint6464
-- ah = 1000 * logBase 10 2

abundentNumbers :: [Int]
abundentNumbers = [x | x <- [1..], sum (properFactors x) >= x] 

fac :: Int -> Int 
fac 0 = 1
fac n = n * fac (n - 1)

fac' :: Integer -> Integer 
fac' 0 = 1
fac' n = n * fac' (n - 1)

sumDigits :: Integer -> Integer 
sumDigits 0 = 0
sumDigits x = (x `mod` 10) + sumDigits (x `div` 10)

facSum :: Integer -> Integer 
facSum n = sumDigits $ fac' n

facHundred :: Integer 
facHundred = sumDigits $ fac' 99

latticePaths :: Integer -> Integer 
latticePaths n = (product [(n+1)..2*n]) `div` (product [1..n])

distinctP :: Integer -> Integer -> Set.Set Integer
distinctP a b = Set.fromList [x^y | x <- [2..a], y <- [2..b]]

distinctPL :: Integer -> Integer -> Int
distinctPL a b = Set.size $ distinctP a b

powDigSum :: Integer -> Integer -> Integer
powDigSum n p = sum $ map (^p) (convertIntegerToArray n)

digitFP :: Integer -> [Integer]
digitFP p = [x | x <- [2..10^top], x == powDigSum x p]
    where 
        top :: Integer
        top = maybe 0 id $ find (\a -> (a * 9^p) < 10^a) [1..]

sumDigitFP :: Integer -> Integer 
sumDigitFP p = sum $ digitFP p

convertArrayToInt :: [Int] -> Int 
convertArrayToInt ns = sum $ zipWith (\n x -> n*10^x) (reverse ns) [0..length ns-1]

rotations :: Int -> [Int]
rotations n = [convertArrayToInt $ take len (drop x cyc) | x <- [1..len]]
    where
    arr = convertIntToArray n
    len = length arr
    cyc = cycle arr 

circularPrimes :: Int -> [Int]
circularPrimes n = [2, 3, 5] ++ [x | x <- takeWhile (<n) primes, divTwo x, divThree x, divFive x, all isPrime (rotations x)]
    where 
        divTwo a = all (not . even) (convertIntToArray a)
        divThree a = (sum (convertIntToArray a)) `mod` 3 /= 0
        divFive a = all (\b -> b /= 0 && b /= 5) (convertIntToArray a) 

sumFactorial :: Integer -> Integer 
sumFactorial n = sum $ map fac' (convertIntegerToArray n)

-- TODO: FIND OUT MAX IT CULD BE?
sumFactorials :: [Integer] 
sumFactorials = [x | x <- [10..10000000], x == sumFactorial x] 

-- 

isTruncatablePrime :: Int -> Bool 
isTruncatablePrime n = all isPrime $ leftTruncated ++ rightTruncated
    where 
        leftTruncated = [ convertArrayToInt (drop x digits) | x <- [0..length digits - 1]]
        rightTruncated = [ convertArrayToInt $ reverse (drop x (reverse digits)) | x <- [0..length digits - 1]]
        digits = convertIntToArray n

truncatablePrimes :: [Int]
truncatablePrimes = take 11 $ [n | n <- (drop 4 primes), removeOnes n, isTruncatablePrime n]
    where 
        removeOnes n = drop (len - 1) digits /= [1] && take 1 digits /= [1]
            where
                digits = convertIntToArray n
                len = length digits

-- permutationsB :: [a] -> [[a]]
-- permutationsB [] = [[]]
-- permutationsB xs = [ map (\as -> x : as ) (permutationsB (delete x xs)) | x <- xs ]
-- permutationsB xs = [ map (x:) [[0],[1],[2]] | x <- xs ]
permutationsB :: Eq a => [a] -> [[a]]
permutationsB [] = [[]]
permutationsB xs = concat [ map (x:) (permutationsB (delete x xs)) | x <- xs ]
          
lexPerms :: Int -> [Int]
lexPerms n = map convertArrayToInt (permutationsB [0..n])

indexLex :: Int -> Int
indexLex n = (lexPerms 9) !! (n-1)

pandigitalNumbers :: [Int]
pandigitalNumbers = concat [map convertArrayToInt (permutations [1..n]) | n <- [1..9]]

pandigitalPrimes :: [Int]
pandigitalPrimes = [n | n <- pandigitalNumbers, isPrime n]

-- TODO
pentagonalNumbers :: [Int]
pentagonalNumbers = [n * (3 * n - 1) `div` 2 | n <- [1..]] 

-- TODO
-- pandigitalMultiples :: [Int]

-- maxQuadPrime :: String
-- maxQuadPrime = maximumBy compareFunc quadPrimes 
--     where
--     compareFunc :: String -> String -> Ordering
--     compareFunc str1 str2 = n1 `compare` n2
--         where n1 = read (takeWhile (/=' ') str1) :: Int
--               n2 = read (takeWhile (/=' ') str2) :: Int


-- quadPrimes :: [String]
-- quadPrimes = map i asbs
--     where 
--         i :: (Int, Int) -> String 
--         i (a, b) = show (h (a, b)) ++ " " ++ show (a, b)
--         h :: (Int, Int) -> Int
--         h (a, b) =  maybe (-1) id (find (\x -> g (a, b) x) [0..])
--         g :: (Int, Int) -> Int -> Bool
--         g (a, b) n = (not . isPrime' . f (a, b)) n
--         f :: (Int, Int) -> Int -> Int
--         f (a, b) n = n^2 + a*n + b
--         asbs :: [(Int, Int)]
--         asbs = [(a, b) | a <- [-999..999], odd a, b <- (takeWhile (<=1000) primes) ++ map negate (takeWhile (<=1000) primes)]

-- quadPrimes' :: [Int]
-- quadPrimes' = map h asbs
--     where 
--         h :: (Int, Int) -> Int
--         h (a, b) =  maybe (-1) id (find (\x -> g (a, b) x) [0..])
--         g :: (Int, Int) -> Int -> Bool
--         g (a, b) n = (not . isPrime' . f (a, b)) n
--         f :: (Int, Int) -> Int -> Int
--         f (a, b) n = n^2 + a*n + b
--         asbs :: [(Int, Int)]
--         asbs = [(a, b) | a <- map negate $ takeWhile (<1000) primes, b <- map negate $ takeWhile (<=1000) primes]

-- quadPrime :: Int -> Int -> Maybe Int 
-- quadPrime a b = find f [0..]
--     where f = not . isPrime' . quad
--           quad n = n^2 + a*n + b

-- PROBLEM 49 

-- pandigital products !!
-- combThingy :: Int -> Int -> [[Int]]
-- combThingy n k = helper [1..n] 4
--     where 
--         helper :: [Int] -> Int -> [[Int]]
--         helper [] m = [[]]
--         helper xs m = concat [ x : combThingy (filter (>= x) xs) (m-1) | x <- xs]

-- x <- [1..9], y <- [1..9]
-- x = 1, y = 1, 1 : combThingy 

primeTest :: [Int]
primeTest = filter (>=1000) $ takeWhile (<10000) primes

combinations :: Int -> [a] -> [[a]]
combinations k xs = filter ((k==) . length) $ subsequences xs

diffs :: [Int] -> [[Int]]
diffs xs = [[y - x | y <- filter (>x) xs] | x <- xs ]

-- PROBLEM 48 

selfPows :: Integer -> Integer
selfPows n = sum [ x^x | x <- [1..n]]

prob48 :: Integer
prob48 = (sum [x^x | x <- [1..999], x /= 10, x /= 100]) `mod` 10^10
