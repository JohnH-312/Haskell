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
fsum' xs = foldl (+) 0 xs

felem :: (Eq a) => a -> [a] -> Bool
felem y ys = foldl (\acc x -> if x == y then True else acc) False ys

fmap :: (a -> b) -> [a] -> [b]
fmap f xs = foldr (\x acc -> f x : acc) [] xs

-- this is far more expensive computationally since ++ is much more difficult than :
fmapl :: (a -> b) -> [a] -> [b]
fmapl f xs = foldl (\acc x -> acc ++ [f x]) [] xs

-- using right folds is useful when building new lists based on 
-- old lists because the : operartor is much less expensive to 
-- use than repetedly doing ++

-- Left folds DON"T work on infinite lists, yet right ones do, this makes sense cus infinity

-- sum can be even simpler with foldl1
easySum :: (Num a) => [a] -> a
easySum = foldl1 (+)

-- here's some standard funcs with fold
maximum' :: (Ord a) => [a] -> a  
maximum' = foldr1 (\x acc -> if x > acc then x else acc)  
  
reverse' :: [a] -> [a]  
reverse' = foldl (\acc x -> x : acc) []  
  
product' :: (Num a) => [a] -> a  
product' = foldr1 (*)  
  
filter' :: (a -> Bool) -> [a] -> [a]  
filter' p = foldr (\x acc -> if p x then x : acc else acc) []  
  
-- this is a pretty dumb one, as it could be simple pattern matching
head' :: [a] -> a  
head' = foldr1 (\x _ -> x)  
  
last' :: [a] -> a  
last' = foldl1 (\_ x -> x) 

-- i'm now reading about $
-- map ($ 3) [(4+), (10*), (^2), sqrt]  
--[7.0,30.0,9.0,1.7320508075688772]  
-- this shows mapping over a list of different functions being applied

-- pointless/point free
easySum2 :: (Num a) => [a] -> a
easySum2 = foldl (+) 0

