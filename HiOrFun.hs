-- example of the convenience of curried statements
compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100 -- we don't need the second term here

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpper :: Char -> Bool
isUpper = (`elem` ['A'..'Z'])

-- using this parenthesis notation means whatever paramter 
-- is applied outside will go to whichever slot is missing inside

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f y x = f x y

-- MAPS!!!

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs
-- Note, this is equivalent in some cases at least to list comprehension

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter' p xs
    | otherwise = filter' p xs

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
    where p x = x `mod` 3829 == 0

--introduced takeWhile, which takes a thing untill some thing ceases to be true

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n:chain (n `div` 2)
    | odd n  = n:chain (n*3 + 1)

--lambdas are completely anonymous functions to be thrown in to avoid un-needed declarations

numLongChains :: Int
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))

lflip :: (a -> b -> c) -> b -> a -> c
lflip f = \x y -> f y x

fsum :: (Num a) => [a] -> a 
fsum xs = foldl (\acc x -> acc + x) 0 xs

fsum' :: (Num a) => [a] -> a 
fsum' = foldl (+) 0

felem :: (Eq a) => a -> [a] -> Bool
felem y ys = foldl (\acc x -> if x == y then True else acc) False ys

fmap :: (a -> b) -> [a] -> [b]
fmap f xs = foldr (\x acc -> f x : acc) [] xs

-- using right folds is useful when building new lists based on 
-- old lists because the : operartor is much less expensive to 
-- use than repetedly doing ++


