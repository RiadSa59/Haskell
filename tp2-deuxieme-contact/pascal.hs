module Pascal where
import Test.QuickCheck
import Text.Show.Functions -- for Show instance for function types

-- TP2: Deuxième contact avec Haskell
--
-- Riad SABIR 

-- Exercice 3

pasPascal :: [Integer] -> [Integer]

pasPascal [] = []
pasPascal (x:xs) = zipWith (+) (1:x:xs) (0:xs++[0])


-- Exercice 4

pascal :: [[Integer]]

pascal = iterate pasPascal [1]
