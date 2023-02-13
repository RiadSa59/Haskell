module Main where 

-- TP2: Deuxième contact avec Haskell
--
-- Riad SABIR 
--
import Graphics.Gloss


-- Courbe du dragon 
-- Exercice 5
pointAintercaler :: Point -> Point -> Point
pointAintercaler p1 p2 = ((fst p1 + fst p2)/2 + (snd p2 - snd p1)/2, 
                        (snd p1 + snd p2)/2 + (fst p1 - fst p2)/2)


-- Exercice 6
pasDragon :: Path -> Path
pasDragon [] = error "the starting path must have at least two points"
pasDragon [_] = error "the starting path must have at least two points"
pasDragon (p1:p2:[]) = [p1,(pointAintercaler p1 p2), p2]
pasDragon (p1:p2:p3:[]) = [p1,(pointAintercaler p1 p2), p2 ,(pointAintercaler p3 p2) , p3]
pasDragon (p1:p2:p3:ps) = p1:(pointAintercaler p1 p2):p2:pointAintercaler p3 p2 :pasDragon(p3:ps) 


-- Exercice 7
dragon :: Point -> Point -> [Path]
dragon p1 p2 = iterate pasDragon [p1, p2]

-- Exercice 8
dragonOrdre :: Point -> Point -> Int -> Path
dragonOrdre p1 p2 0 = [p1,p2]
dragonOrdre p1 p2 n = pasDragon(dragonOrdre p1 p2 (n-1))

-- main program
main :: IO()
main = animate (InWindow "Dragon" (500, 500) (0, 0)) white (dragonAnimeOrdre (50,250) (450,250))


dragonAnime :: RealFrac a => Point -> Point -> a -> Picture
dragonAnime a b t = Line (dragon a b !! (round t `mod` 20))

dragonAnimeOrdre :: RealFrac a => Point -> Point -> a -> Picture
dragonAnimeOrdre a b t = Line ( dragonOrdre a b (round t `mod` 15))
