import Control.Monad
import Data.Char

{- 1. Here is a very simple, short block of code. Notice it has a forever
that will make it keep running, over and over again. Load it into
your REPL and test it out. Then refer back to the chapter and
modify it to exit successfully a്er a False result. -}

{- 2. If you tried using palindrome on a sentence such as “Madam
I’m Adam,” you may have noticed that palindrome checker
doesn’t work on that. Modifying the above so that it works on
sentences, too, involves several steps. You may need to refer
back to previous examples in the chapter to get ideas for proper
ordering and nesting. You may wish to import Data.Char to use
the function toLower -}

purify :: String -> String
purify []     = []
purify (x:xs) = case elem (toLower x) ['a'..'z'] of True  -> (toLower x)  : purify xs
                                                    False ->                purify xs

main :: IO ()
main = forever $ do
  line1 <- getLine
  case ((purify line1) == reverse (purify line1)) of
    True  -> putStrLn "It's a palindrome!"
    False -> putStrLn "Nope!"