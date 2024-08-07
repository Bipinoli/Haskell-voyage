factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], n `mod` x == 0]

isPrime :: Int -> Bool
isPrime n = factors n == [1, n]

isSorted :: [Int] -> Bool
isSorted xs = and [x <= y | (x, y) <- zip xs (tail xs)]

mylen :: [a] -> Int
mylen = foldr (\_ acc -> acc + 1) 0

myreverse :: [a] -> [a]
myreverse = foldr (\x y -> y ++ [x]) []

myappend :: [a] -> [a] -> [a]
myappend = foldr (:)

starPrinter :: Int -> IO ()
starPrinter n = do
  if n > 0
    then do
      putStrLn $ take n $ repeat '*'
      starPrinter (n - 1)
    else
      return ()

primes = sieve [2 ..]

sieve :: [Int] -> [Int]
sieve (x : xs) = x : sieve (filter (\l -> l `mod` x /= 0) xs)

-- $! => strict evaulation (eager evaluation). Disable lazy evulation and evaluate immediately

sumWith :: [Int] -> Int -> Int
sumWith [] v = v
sumWith (x : xs) v = sumWith xs $! (x + v)
