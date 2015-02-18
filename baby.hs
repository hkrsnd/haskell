lucky :: Int -> String
lucky 7 = "LUCKY NUMBER SEVEN"
lucky x = "Sorry, you're out of luck, pal!"

sayMe :: Int -> String
sayMe 1 = "One"
sayMe 2 = "Two"
sayMe 3 = "Three"
sayMe 4 = "Four"
sayMe 5 = "Five"
sayMe x = "Not between 1 and 5"

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial(n-1)

cylinder :: Double -> Double -> Double
cylinder r h =
  let sideArea = 2*pi*r*h
      topArea = pi*r^2
  in  sideArea + 2 * topArea

replicate' :: Int -> a -> [a]
replicate' n x
  | n <= 0  = []
  | otherwise = x : replicate' (n-1) x

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerOrEqual = [a | a<-xs, a<=x]
      larger = [a | a <- xs, a > x]
  in  quicksort smallerOrEqual ++ [x] ++ quicksort larger

compareWithHundred :: Int -> Ordering
compareWithHundred x = compare 100 x

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y = f y x

largestDivisible :: Integer
largestDivisible = head (filter p [100000, 99999 ..])
  where p x = x `mod` 3829 == 0

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
  where isLong xs = length xs > 15


import Data.List

wordNums :: String -> [(String,Int)]
wordNums = map(\ws -> (head ws, length ws)) . group . sort . words
