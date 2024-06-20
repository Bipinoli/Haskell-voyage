factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_ : rest) = 1 + length' rest

firstLetter :: String -> String
firstLetter "" = error "empty string"
firstLetter all@(x : xs) = "The first letter of '" ++ all ++ "' is " ++ [x]

bmiIndex :: (RealFloat f) => f -> f -> String
bmiIndex height weight
  | bmi <= skinny = "underweight"
  | bmi <= normal = "normal"
  | bmi <= overweight = "overweight"
  | otherwise = "morbidly overweight"
  where
    bmi = weight / height ^ 2
    skinny = 18.5
    normal = 25.0
    overweight = 30.0

maxFunc :: (Ord a) => a -> a -> a
maxFunc x y
  | x < y = y
  | otherwise = x

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
  let smaller = quicksort [k | k <- xs, k <= x]
      bigger = quicksort [k | k <- xs, k > x]
   in smaller ++ [x] ++ bigger

add :: Integer -> Integer -> Integer
add x y = x + y

myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f (x : xs) = f x : myMap f xs

fibonacci = 1 : 1 : [x + y | (x, y) <- zip fibonacci (tail fibonacci)]
