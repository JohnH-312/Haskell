doubleMe x = x + x
doubleUs x y = x * 2 + y * 2
doubleSmallNum x = if x > 100
                    then x
                    else x*2
conan = "HEY it's Conan"

removeNonUpperCase :: [Char] -> [Char]
removeNonUpperCase st = [c | c <- st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

factorial :: Integer -> Integer
factorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' ::Double -> Double
circumference' r = 2 * pi * r

sayMe :: (Integral a) => a -> String
sayMe 1 = "One"
sayMe 2 = "Two"
sayMe x = "Not one or two"

recFactorial :: Integer -> Integer
recFactorial 0 = 1
recFactorial x = x * recFactorial (x-1)

badVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
badVectors a b = (fst a + fst b, snd a + snd b)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c 
third (_, _, z) = z 

getName = do 
    putStrLn "What is your name?"
    username <- getLine
    putStrLn $ "Hello, " ++ username ++ "!"

tell :: (Show a) => [a] -> String  
tell [] = "The list is empty"  
tell (x:[]) = "The list has one element: " ++ show x  
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y  
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y  

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String  
capital "" = "Empty string, whoops!"  
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x] 

bmiTell :: (RealFloat a) => a -> String  --this is how guards work
bmiTell bmi  
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"  

bmiTell' :: (RealFloat a) => a -> a -> String  
bmiTell' weight height  
    | bmi<= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat = "You're fat! Lose some weight, fatty!"  
    | otherwise                 = "You're a whale, congratulations!" 
    where   bmi = weight / height ^ 2     -- this is sorta a locally defined function/name
            (skinny, normal, fat) = (18.5, 25.0, 30.0)

max' :: (Ord a) => a -> a -> a  
max' a b   
    | a > b     = a  
    | otherwise = b     

myCompare :: (Ord a) => a -> a -> Ordering  
a `myCompare` b  
    | a > b     = GT  
    | a == b    = EQ  
    | otherwise = LT 

--this helps illustrate how wheere functions can use pattern matching
initials :: String -> String -> String
initials first last = [f] ++ ". " ++ [l] ++ "."
    where   (f:_) = first
            (l:_) = last

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h| (w,h) <- xs]
    where bmi weight height = weight / height ^ 2

calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' xs = [bmi | (w,h) <- xs, let bmi = w / h ^ 2]

cylinder :: (RealFloat a) => a -> a -> a    -- with a let x in y
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea

cylinder' :: (RealFloat a) => a -> a -> a  -- with a where clause
cylinder' r h = sideArea + 2 * topArea
    where   sideArea = 2 * pi * r * h  
            topArea = pi * r ^2  

head' :: [a] -> a
head' [] = error "So no head!!!"
head' (x:_) = x

head2' ::[a] -> a 
head2' xs = case xs of  [] -> error "So no head!!"
                        (x:_) -> x

-- with case statement
describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of [] -> "empty."  
                                               [x] -> "a singleton list."   
                                               xs -> "a longer list." 

-- no case version
describeList' :: [a] -> String  
describeList' xs = "The list is " ++ what xs  
    where what [] = "empty."  
          what [x] = "a singleton list."  
          what xs = "a longer list."



