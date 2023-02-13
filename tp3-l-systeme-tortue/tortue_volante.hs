module Main where

import Graphics.Gloss


-- TP3: Le L-système et la Tortue - 2ème partie
--
-- Riad SABIR 

-- This part contains only the flying turtle

--IMPLEMENTATION

type Symbole  = Char
type Mot      = [Symbole]
type Axiome   = Mot
type Regles   = Symbole -> Mot
type LSysteme = [Mot]

-- Exercice 1

regleKoch :: Symbole -> Mot
regleKoch letter | letter == 'F' = "F-F++F-F"
                 | letter == '-' = "-" 
                 | letter == '+' = "+"
                 | otherwise     = "the symbol isn't defined"

motSuivant :: Regles -> Mot -> Mot --Recursivity

motSuivant _ [] = [] -- Base case, if the initial word is empty, returns the final word
motSuivant regles (firstLetter:restOfTheWord) = (regles firstLetter)++(motSuivant regles restOfTheWord)

motSuivant' :: Regles -> Mot -> Mot --Comprehension
motSuivant' regles word = concat([regles(letter) | letter <- word])

motSuivant'' :: Regles -> Mot -> Mot --Prelude function
motSuivant'' regles word = concat(map (regles) word)


lsysteme :: Axiome -> Regles -> LSysteme
lsysteme axiome regles = iterate (motSuivant'' regles) axiome 

--TORTUE

type EtatTortue = (Point, Float)
type Config = (EtatTortue -- État initial de la tortue
              ,Float      -- Longueur initiale d’un pas
              ,Float      -- Facteur d’échelle
              ,Float      -- Angle pour les rotations de la tortue
              ,[Symbole]) -- Liste des symboles compris par la tortue


etatInitial :: Config -> EtatTortue
etatInitial (x,_,_,_,_) = x

longueurPas :: Config -> Float
longueurPas (_,x,_,_,_) = x

facteurEchelle :: Config -> Float
facteurEchelle (_,_,x,_,_) = x

angle :: Config -> Float
angle (_,_,_,x,_) = x

symbolesTortue :: Config -> [Symbole]
symbolesTortue (_,_,_,_,x) = x


avance :: Config -> EtatTortue -> EtatTortue --update the state of the turtle after one step. It's determinized by the configuration
avance config ((x,y),cap) = ((x',y'),cap)
              where x' = x + (longueurPas config) * cos(cap) -- x + d * cos (cap)
                    y' = y + (longueurPas config) * sin(cap) -- y + d * sin (cap)
  

tourneAGauche :: Config -> EtatTortue -> EtatTortue
tourneAGauche config ((x,y),cap) = ((x,y),cap')
              where cap' = cap + (angle config) -- cap + a

tourneADroite :: Config -> EtatTortue -> EtatTortue
tourneADroite config ((x,y),cap) = ((x,y),cap')
              where cap' = cap - (angle config) -- cap - a


member :: Char -> [Char] -> Bool
member e xs = foldr (\x acc -> acc || e == x) False xs


filtreSymbolesTortue :: Config -> Mot -> Mot
filtreSymbolesTortue config mot = [validSymbol | validSymbol <- mot , member validSymbol (symbolesTortue config)]

type EtatDessin = ([EtatTortue], [Path])

interpreteSymbole :: Config -> EtatDessin -> Symbole -> EtatDessin
interpreteSymbole cfg ((s:xs), (p:ps)) 'F' =
  let s2 = avance cfg s in
  ((s2:xs), (p++[fst s2]) : ps)    
interpreteSymbole cfg ((s:xs), (p:ps)) '+' =
  let s2 = tourneAGauche cfg s in
  ((s2:xs), (p++[fst s2]) : ps)
interpreteSymbole cfg ((s:xs), (p:ps)) '-' =
  let s2 = tourneADroite cfg s in
  ((s2:xs), (p++[fst s2]) : ps)
interpreteSymbole cfg ((s:xs),ps) '[' =
  ((s:s:xs),[fst s]:ps)
interpreteSymbole cfg ((s:s2:xs),ps) ']' =
  (s2:xs, [fst s2]:ps)
interpreteSymbole _ _ _ = error "unknown symbol"



interpreteMot :: Config -> Mot -> Picture
interpreteMot cfg mot =
  let i = etatInitial cfg in
  pictures (map line (interpreteMot_rec cfg ([i],[[fst i]]) (filtreSymbolesTortue cfg mot)))
  where
    interpreteMot_rec _ (s,p) [] = p
    interpreteMot_rec cfg (s,p) (x:xs) =
      let r = interpreteSymbole cfg (s,p) x in
      interpreteMot_rec cfg r xs


lsystemeAnime :: LSysteme -> Config -> Float -> Picture
lsystemeAnime xs config t = interpreteMot config' (xs !! enieme)
  where enieme = round t `mod` 6
        config' = case config of
          config -> ((etatInitial config), (longueurPas config) * ((facteurEchelle config) ^ enieme), (facteurEchelle config), (angle config), (symbolesTortue config))



brindille :: LSysteme
brindille = lsysteme "F" regles
    where regles 'F' = "F[-F]F[+F]F"
          regles  s  = [s]

broussaille :: LSysteme
broussaille = lsysteme "F" regles
    where regles 'F' = "FF-[-F+F+F]+[+F-F-F]"
          regles  s  = [s]


brindilleAnime :: Float -> Picture
brindilleAnime = lsystemeAnime brindille (((0, -400), pi/2), 800, 1/3, 25*pi/180, "F+-[]")

broussailleAnime :: Float -> Picture
broussailleAnime = lsystemeAnime broussaille (((0, -400), pi/2), 500, 2/5, 25*pi/180, "F+-[]")


main :: IO()
main = animate (InWindow "L-système" (1000, 1000) (0, 0)) white broussailleAnime

