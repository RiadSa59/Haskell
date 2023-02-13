module ExercicesTP2 where

import Test.QuickCheck
import Graphics.Gloss
import Text.Show.Functions -- for Show instance for function types

-- TP2: Deuxième contact avec Haskell
--
-- Riad SABIR 
--

-- Exercice 1
alterne :: [a] -> [a]
alterne [] = []
alterne (x:[]) = [x]
alterne (x:xs:xss) = x:alterne xss


-- Exercice 2 
combine :: (a -> b -> c) -> [a] -> [b] -> [c]
combine f [] ys = []
combine f xs [] = []
combine f (x:xs) (y:ys) = [f x y ] ++ combine f xs ys  

prop_combine f xs ys = combine f xs ys == zipWith f xs ys
-- in GHCi : "Prelude> quickCheck prop_combine"



