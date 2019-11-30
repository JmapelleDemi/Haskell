{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

instance TooMany (Int,Int) where
    tooMany (x,y) = (x+y) > 42

instance TooMany (Int,String) where
    tooMany (n,str) = n > 42

instance (Num a, TooMany a) => TooMany (a,a) where
    tooMany (x,y) = tooMany x && tooMany y

newtype Goats = Goats Int deriving (Eq, Show, TooMany)

data Price =
    Price Integer deriving (Eq, Show)

data Size =
    Size Integer deriving (Eq, Show)

data Manufacturer = Mini
                  | Mazda
                  | Tata
                  | Volkswagen
                  deriving (Eq, Show)

data Airline = PapuAir
             | CatapultsR'Us
             | TakeYourChancesUnited
             deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
             | Plane Airline Size
             deriving (Eq, Show)

data OperatingSystem = GnuPlusLinux
                     | OpenBSD
                     | Mac
                     | Windows
                     deriving (Eq, Show)

data ProgrammingLanguage = Haskell
                         | Agda
                         | Idris
                         | PureScript
                         deriving (Eq, Show)

data Programmer = Programmer { os   :: OperatingSystem ,
                               lang :: ProgrammingLanguage }
                                  deriving (Eq, Show)

data Quantum = Yes
             | No
             | Both
             deriving (Eq, Show)

data Quad = One
          | Two
          | Three
          | Four
          deriving (Eq, Show)

data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
                  deriving (Eq, Ord, Show)

myCar    = Car Mini (Price 14000)
urCar    = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge     = Plane PapuAir (Size 100)
wasCar   = Car Volkswagen (Price 800)

-- Define function (iCar, isPlane, areCars)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _         = False 

areCars :: [Vehicle] -> [Bool]
areCars = foldr check []
          where
          check x y = case x of (Car _ _) -> [True]  ++ y
                                otherwise -> [False] ++ y

-- Write a function to tell us the manufacturer od a piece of Data

getManu :: Vehicle -> Manufacturer
getManu (Car manu _) = manu

{- Write a function that generates all possible values of Programmer.
Use the provided lists of inhabitants of 'OperatingSystem' and
'ProgrammingLanguage' -}

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [GnuPlusLinux, OpenBSD, Mac, Windows]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer x y | x <- allOperatingSystems, y <- allLanguages]

{- According to the equality of 'a -> b' abd 'b^a' there should be 2^3
or 8 implementations of this (convert :: Quantum -> Bool) function.
Does this hold? Write it out and prove it fo yourself -}

convert :: Quantum -> Bool
convert Yes  = True
convert No   = True 
convert Both = True

convert2 :: Quantum -> Bool
convert2 Yes  = False
convert2 No   = False
convert2 Both = False       -- 2 impl.

convert3 :: Quantum -> Bool
convert3 Yes  = True
convert3 No   = False
convert3 Both = False       -- 3

convert4 :: Quantum -> Bool
convert4 Yes  = True
convert4 No   = True
convert4 Both = False       -- 4

convert5 :: Quantum -> Bool
convert5 Yes  = False
convert5 No   = True
convert5 Both = True        -- 5

convert6 :: Quantum -> Bool
convert6 Yes  = False
convert6 No   = False
convert6 Both = True        -- 6

convert7 :: Quantum -> Bool
convert7 Yes  = False
convert7 No   = True
convert7 Both = False       -- 7    

convert8 :: Quantum -> Bool
convert8 Yes  = True
convert8 No   = False
convert8 Both = True  

{- Given the definition of 'BinaryTree' above, write a map function
for the data structure -}

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right) 

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

{- Write functions to convert 'BynaryTree' values to lists. Make certain
your implementation passes the tests -}

preorder :: BinaryTree a -> [a]
preorder Leaf                = []
preorder (Node left a right) = a : ( (preorder left) ++ (preorder right) )

inorder :: BinaryTree a -> [a]
inorder Leaf                = []
inorder (Node left a right) = (inorder left) ++ [a] ++ (inorder right)

postorder :: BinaryTree a -> [a]
postorder Leaf                = []
postorder (Node left a right) = (postorder left) ++ (postorder right) ++ [a]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder = if preorder testTree == [2,1,3]
               then putStrLn "Preorder works"
               else putStrLn "Preorder fails"

testInorder :: IO ()
testInorder = if inorder testTree == [1,2,3]
              then putStrLn "Inorder works"
              else putStrLn "Inorder fails"

testPostorder :: IO ()
testPostorder = if postorder testTree == [1,3,2]
                then putStrLn "Postorder works"
                else putStrLn "Postorder fails"

main :: IO ()
main = do
    testPreorder
    testInorder
    testPostorder

-- Write foldr for BynaryTree. Any traversal order is fine

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f ini Leaf = ini
foldTree f ini (Node left a right) =
    foldTree f (f a (foldTree f ini left)) right  -- inorder