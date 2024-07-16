-- Question 1
toDigits :: Integer -> [Integer]
toDigits <= 0 = []
toDigits = map (read . return ) . show

toDigitsRev :: Integer -> [Integer]
toDigitsRev = toDigits . read . reverse . show

-- Question 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs =  map (\(x, y) -> x * odds y) (zip xs [1..])

odds :: Integer -> Integer
odds x = x `mod` 2 + 1

-- Question 3
sumDigits :: [Integer] -> Integer
sumDigits = foldl (\acc x -> acc + sumDigit x) 0

sumDigit :: Integer -> Integer
sumDigit x
 | x < 10 = x
 -- "ab" = 10a + b, hence a + b = "ab" - 9a
 | otherwise = x - 9 * read [head (show x)]

-- Question 4
validate :: Integer -> Bool
validate = modcheck . sumDigits . doubleEveryOther . toDigits

modcheck :: Integer -> Bool
modcheck x = x `mod` 10 == 0
