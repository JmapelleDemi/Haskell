import Control.Applicative
import Data.Maybe hiding (fromMaybe)

x = [1,2,3]
y = [4,5,6]
z = [7,8,9]

{- The next thing we want to do is write some functions that zip
those lists together and uses lookup to ﬁnd the value associated with
a speciﬁed key in our zipped lists. For demonstration purposes, it’s
nice to have the outputs be predictable, so we recommend writing
some that are concrete values, as well as one that can be applied to a
variable:

lookup :: Eq a => a -> [(a, b)] -> Maybe b    -}

myZip :: [a] -> [b] -> [(a,b)]
myZip xs ys = getZipList $ (,) <$>ZipList xs <*> ZipList ys
-- Demi: I didn't know in that moment that I invented a bycicle, as we see soon

-- zip x and y using 3 as the lookup key
xs :: Maybe Integer
xs = lookup 3 $ myZip x y

-- zip y and z using 6 as the lookup key
ys :: Maybe Integer
ys = lookup 6 $ myZip y z 

-- it's also nice to have one that
-- will return Nothing, like this one
-- zip x and y using 4 as the lookup key
zs :: Maybe Integer
zs = lookup 4 $ zip x y

-- now zip x and z using a variable lookup key
z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

{- Now we want to add the ability to make a Maybe (,) of values using
Applicative. Have x1 make a tuple of xs and ys, and x2 make a tuple of
of ys and zs. Also, write x3 which takes one input and makes a tuple
of the results of two applications of z' from above. -}

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys
x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs
x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 n = ( ( z' n ) , ( z' n ) )

{- Next, we’re going to make some helper functions. Let’s use uncurry
to allow us to add the two values that are inside a tuple:
CHAPTER 22. FUNCTIONS WAITING FOR INPUT 858

uncurry :: (a -> b -> c) -> (a, b) -> c  -}
-- that first argument is a function
-- in this case, we want it to be addition
-- summed is just uncurry with addition as
-- the first argument

summed :: Num c => (c, c) -> c
summed = uncurry (+)

{- And now we’ll make a function similar to some we’ve seen before
that lifts a boolean function over two partially-applied functions:  -}

bolt :: Integer -> Bool
-- use &&, >3, <8
bolt = (&&) <$> (>3) <*> (<8) 

{- Finally, we’ll be using fromMaybe in the main exercise, so let’s look
at that:

fromMaybe :: a -> Maybe a -> a  -}

{- You give it a default value and a Maybe value. If the Maybe value
is a Just a, it will return the ∧ value. If the Maybe value is a Nothing,
it returns the default value instead   -}

fromMaybe :: a -> Maybe a -> a
fromMaybe def (Just a) = a
fromMaybe def Nothing  = def

{- Now we’ll cobble together a main function, so that in one function
call we can execute several things at once -}

main :: IO ()
main = do
  print $ sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z
  print $ sequenceA [(>3), (<8), even] 7
  print $ newFold (&&) True 5 
  print appSeq
  print appSeq'
  print appBolt

{- We have a Reader for the Applicative (functions) and a traversable
for the list. Pretty handy. We’re going to call that function sequA for
the purposes of the following exercises:   -}

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m

-- And henceforth let        summed <$> ((,) <$> xs <*> ys)      be known as s'.

s' = summed <$> ((,) <$> xs <*> ys)

{- OK, your turn. Within the main function above, write the following
(you can delete everything after do now if you prefer — just remember
to use print to be able to print the results of what you’re adding):

1. fold the boolean conjunction operator over the list of results of
sequA (applied to some value)     -}

newFold op ini m = foldr op ini (sequA m)

-- 2. apply sequA to s'; you’ll need fromMaybe

appSeq :: [Bool]
appSeq = sequA $ fromMaybe 0 s'
appSeq' :: Maybe [Bool]
appSeq' = sequA <$> s' -- appSeq' ::

-- 3. apply bolt to ys; you’ll need fromMaybe

appBolt = bolt $ fromMaybe 0 ys
