data Shape = Circle Point Float | Rectangle Point Point deriving (Show)
data Point = Point Float Float deriving (Show)


surface :: Shape -> Float
surface (Circle _ _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1 + a)(y1+b)) (Point (x2+a)(y2+b))


--default constructors
baseCircle :: Float -> Shape  
baseCircle r = Circle (Point 0 0) r  
  
baseRect :: Float -> Float -> Shape  
baseRect width height = Rectangle (Point 0 0) (Point width height)


-- Record Syntax
--      this defines functions which get the value from the type
data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     , height :: Float  
                     , phoneNumber :: String  
                     , flavor :: String  
                     } deriving (Show)   
data Car = Car {company :: String, model :: String, year :: Int} deriving (Show) 

tellCar :: Car -> String  
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y 

-- Type Paramatization
data Vector a = Vector a a a deriving (Show)  
  
vplus :: (Num t) => Vector t -> Vector t -> Vector t  
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)  
  
vectMult :: (Num t) => Vector t -> t -> Vector t  
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)  
  
scalarMult :: (Num t) => Vector t -> Vector t -> t  
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n  

--Type aliases/synonyms
type PhoneNumber = String  
type Name = String  
type PhoneBook = [(Name,PhoneNumber)]  

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool  
inPhoneBook name pnumber pbook = (name,pnumber) `elem` pbook 

--Typeclasses
data TrafficLight = Red | Yellow | Green  

instance Eq TrafficLight where  
    Red == Red = True  
    Green == Green = True  
    Yellow == Yellow = True  
    _ == _ = False  
instance Show TrafficLight where  
    show Red = "Red light"  
    show Yellow = "Yellow light"  
    show Green = "Green light" 


class YesNo a where  
    yesno :: a -> Bool  

instance YesNo Int where  
    yesno 0 = False  
    yesno _ = True  
instance YesNo [a] where  
    yesno [] = False  
    yesno _ = True  
instance YesNo Bool where  
    yesno = id   
instance YesNo (Maybe a) where  
    yesno (Just _) = True  
    yesno Nothing = False  
instance YesNo (Tree a) where  
    yesno EmptyTree = False  
    yesno _ = True  
instance YesNo TrafficLight where  
    yesno Red = False  
    yesno _ = True  

-- FUNCTORS
data Either a b = Left a | Right b  

class Functor f where  
    fmap :: (a -> b) -> f a -> f b  

-- this should be familiar, as its already the default way map works
instance Functor [] where  
    fmap = map
instance Functor Maybe where  
    fmap f (Just x) = Just (f x)  
    fmap f Nothing = Nothing  
instance Functor Tree where  
    fmap f EmptyTree = EmptyTree  
    fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)  
instance Functor (Either a) where  
    fmap f (Right x) = Right (f x)  
    fmap f (Left x) = Left x  

-- Time to get wacky
class Tofu t where  
    tofu :: j a -> t a j 