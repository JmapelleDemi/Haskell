import Data.Char
import Data.List

type Values = String
data Button = Button { button :: Digit, symbols :: Values } deriving (Eq, Show)
data DaPhone = DaPhone [Button] deriving (Eq, Show)

phonePad :: DaPhone
phonePad = DaPhone [ Button '1' "1",
                     Button '2' "abc2",
                     Button '3' "def3",
                     Button '4' "ghi4",
                     Button '5' "jkl5",
                     Button '6' "mno6",
                     Button '7' "pqrs7",
                     Button '8' "tuv8",
                     Button '9' "wxyz9",
                     Button '0' "+ 0",
                     Button '*' "^",
                     Button '#' ".,\n" ]

{-- Convert the following converstion into the keypress required to express them.
We're going to suggest types and functions to fill in order to accomplish goal,
but they're not obligatory. If you want to do it differently... you do you -}

convo :: [String]
convo = [ "Wanna play 20 question",
          "ya",
          "U 1st haha",
          "Lol ok. Have u ever tated alcohol lol",
          "Lol ya",
          "Hah thanks just making sure rofl ur turn"]

-- validButtons = "1234567890*#"
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps phone char = if findButton (toLower char) /= Nothing
                         then case isUpper char of True  -> ('*', 1) : (reverseTaps phone (toLower char))
                                                   False ->  [(((button.unJustButton.findButton) char),(countTap char (unJustButton (findButton char))))]
                         else error "false input"

-- assuming the default phone definition
-- 'a' -> [('2',1)]
-- 'A' -> [('*',1),('2',1)]

unJustButton :: Maybe Button -> Button
unJustButton (Just button) = button

countTap :: Char -> Button -> Int
countTap char (Button c list) = check char list
                                where
                                check ch []     = 0
                                check ch (x:xs) = case ch == x of True  -> 1
                                                                  False -> 1 + check ch xs

findButton :: Char -> Maybe Button
findButton char = check char phonePad
                  where
                  check char (DaPhone []) = Nothing
                  check char (DaPhone xs) = if elem char (symbols (head xs))
                                            then Just (head xs)
                                            else check char (DaPhone (tail xs))

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead daphone []     = []
cellPhonesDead daphone (x:xs) = (reverseTaps daphone x) ++ (cellPhonesDead daphone xs)

-- How many times do digits need to be pressed for each message?

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps []           = 0
fingerTaps ((d,p):rest) = p + fingerTaps rest

{- What was the most popular letter for each message? What was its cost? You'll want
to combine 'reverseTaps' and 'fingerTaps' to figure out what it cost in taps.
'reverseTaps' is a list because you need to press a different button in order
to get capitals -}

mostPopularLetter :: String -> Char
mostPopularLetter text = counter ('a',0) ( (doList . sortAndDel) (map toLower text))
                         where
                         counter (char,n) []                 = char
                         counter (char1,n1) [(char2, n2)]    = if n1 > n2 then char1 else char2
                         counter (char1,n1) ((char2, n2):xs) = if n1 > n2 then counter (char1,n1) xs else counter (char2,n2) xs
                         doList [] = []
                         doList (x:xs) = (check (x,0) xs) : doList (xs)
                                         where
                                         check (x,i) []     = (x,i)
                                         check (x,i) (z:zs) = if x == z then check (x,i+1) zs
                                                                        else check (x,i) zs
                         
sortAndDel :: String -> String
sortAndDel [] = []
sortAndDel list = del . sort $ list 
                  where
                  del []     = []
                  del (x:xs) = if ' ' == x then del xs
                                           else x : del xs

mostPopularLetterCost :: String -> Int
mostPopularLetterCost text = let mpl = mostPopularLetter text
                             in nowCount mpl $ how mpl text
                             where
                             how char []     = []
                             how char (x:xs) = if char == x then x : (how char xs) else (how char xs)
                             nowCount char []     = 0
                             nowCount char (x:xs) = (countTap char .unJustButton.findButton.mostPopularLetter) text  + (nowCount char xs)

{- What was the most popular letter overall? What was the most popular word? -}

coolestLtr :: [String] -> Char
coolestLtr biglist = mostPopularLetter . concat $ biglist

coolestWord :: [String] -> String
coolestWord [[]]    = []
coolestWord biglist = whichWord ("word",0) $ search $ words $ delPoints $ map toLower $ unwords $ biglist
                      where
                      delPoints []     = []
                      delPoints (x:xs) = if x == '.' then delPoints xs
                                                     else x : delPoints xs 
                      search []     = []
                      search (x:xs) = (counter (x,0) xs) : (search xs)
                                      where
                                      counter (word,n) []     = (word,n)
                                      counter (word,n) (x:xs) = if word == x then counter (word,n+1) xs
                                                                             else counter (word,n)   xs
                      whichWord (word1,n1) []              = word1
                      whichWord (word1,n1) [(word2,n2)]    = if n1 > n2 then word1 else word2
                      whichWord (word1,n1) ((word2,n2):xs) = if n1 > n2 then whichWord (word1,n1) xs else whichWord (word2,n2) xs
