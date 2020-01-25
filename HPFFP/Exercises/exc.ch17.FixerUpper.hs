{- Exercise: Fixer Upper
Given the function and values provided, use (<$>) from Functor,
(<*>) and pure from the Applicative typeclass to ﬁll in missing bits of
the broken code to make it work 

1. const <$> Just "Hello" <*> "World" -}
x = const <$> Just "Hello" <*> pure "World"

--2. (,,,) Just 90 <*> Just 10 Just "Tierness" [1, 2, 3]
y = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]