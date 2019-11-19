data Person = Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

data Bood = Blah
          | Woot deriving (Eq, Show)

settleDown :: Bood -> Bood
settleDown x = if x == Woot
               then Blah
               else x

type Subject = String
type Verb    = String
type Object  = String

data Sentence =
    Sentence Subject Verb Object
    deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

data Rocks = 
    Rocks String deriving (Eq, Show)

data Yeah =
   Yeah Bool deriving (Eq, Show)

data Papu =
    Papu Rocks Yeah
    deriving (Eq, Show)

truth = Papu (Rocks "chomskydoz") (Yeah True)

equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'
