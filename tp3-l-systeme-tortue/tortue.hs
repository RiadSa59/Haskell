module Main where

import Graphics.Gloss


-- TP3: Le L-système et la Tortue - 1ere partie
--

-- Riad Sabir

-- This part contains only the 11 first answers, the second part is about the flying turtle

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

-- Exercice 2

{- Size of each word

*Exercices> length ((take 4 (lsysteme "F" regleKoch)) !! 0)
8
it :: Int
*Exercices> length ((take 4 (lsysteme "F" regleKoch)) !! 1)
36
it :: Int
*Exercices> length ((take 4 (lsysteme "F" regleKoch)) !! 2)
148
it :: Int
*Exercices> length ((take 4 (lsysteme "F" regleKoch)) !! 3)
596
it :: Int

-}

-- Exercice 3

lsysteme :: Axiome -> Regles -> LSysteme
lsysteme axiome regles = iterate (motSuivant'' regles) axiome 


--TORTUE

type EtatTortue = (Point, Float)
type Config = (EtatTortue -- État initial de la tortue
              ,Float      -- Longueur initiale d’un pas
              ,Float      -- Facteur d’échelle
              ,Float      -- Angle pour les rotations de la tortue
              ,[Symbole]) -- Liste des symboles compris par la tortue

-- Exercice 4
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


-- Exercice 5
avance :: Config -> EtatTortue -> EtatTortue --update the state of the turtle after one step. It's determinized by the configuration
avance config ((x,y),cap) = ((x',y'),cap)
              where x' = x + (longueurPas config) * (cos cap) -- x + d * cos (cap)
                    y' = y + (longueurPas config) * (sin cap) -- y + d * sin (cap)
  
-- Exercice 6

tourneAGauche :: Config -> EtatTortue -> EtatTortue
tourneAGauche config ((x,y),cap) = ((x,y),cap')
              where cap' = cap + (angle config) -- cap + a

tourneADroite :: Config -> EtatTortue -> EtatTortue
tourneADroite config ((x,y),cap) = ((x,y),cap')
              where cap' = cap - (angle config) -- cap - a

--Exercice 7
member :: Char -> [Char] -> Bool
member e xs = foldr (\x acc -> acc || e == x) False xs

filtreSymbolesTortue :: Config -> Mot -> Mot
filtreSymbolesTortue _ [] = []
filtreSymbolesTortue config mot |  member (head mot) (symbolesTortue config) = [head(mot)] ++ (filtreSymbolesTortue config (tail mot))
                                |  otherwise = (filtreSymbolesTortue config (tail mot))

filtreSymbolesTortue' :: Config -> Mot -> Mot
filtreSymbolesTortue' config mot = [validSymbol | validSymbol <- mot , member validSymbol (symbolesTortue config)]

type EtatDessin = (EtatTortue, Path)

--Exercice 8

interpreteSymbole :: Config -> EtatDessin -> Symbole -> EtatDessin
  
interpreteSymbole config (((x,y),cap),(path)) symbol | symbol == '+' = (tourneAGauche config ((x,y),cap),path)
                                                     | symbol == '-' = (tourneADroite config ((x,y),cap),path)
                                                     | symbol == 'F' = (avance config ((x,y),cap),path++[(x,y),(x',y')])
                                                   where ((x',_),_)  = avance config ((x,y),cap)
                                                         ((_,y'),_)  = avance config ((x,y),cap)
                                                      

-- Exercice 9

{-

Dans la fonction interpreteSymbole, avez-vous ajouté en tête ou en queue le nouveau point dans le chemin ? Cela a-t-il une conséquence d’un point de vue complexité (quand la taille du chemin est importante) ?

L'ajout s'effectue en queue, et uniquement s'il s'agit d'un déplacement (pas d'une modification de l'angle). la complexité d'un ajout en queue de liste est en θ(n).
Cela a donc une conséquence lorsque la taille du chemin est importante.

-}

-- Exercice 10

fonctionIntermediaire :: Config -> EtatDessin -> Mot -> EtatDessin
fonctionIntermediaire _ (((x,y),cap),(path)) [] = (((x,y),cap),(path))
fonctionIntermediaire config (((x,y),cap),(path)) (letter:restOfTheWord) = fonctionIntermediaire config (interpreteSymbole config (((x,y),cap),(path)) letter) restOfTheWord

interpreteMot :: Config -> Mot -> Picture
interpreteMot config (letter:restOfTheWord) = line path
                             where (_,path) = fonctionIntermediaire config ((etatInitial config),[]) (filtreSymbolesTortue' config (letter:restOfTheWord))

dessin :: Picture
dessin = interpreteMot (((-150,0),0),100,1,pi/3,"F+-") "F+F--F+F"



-- Exercice 11
lsystemeAnime :: LSysteme -> Config -> Float -> Picture
lsystemeAnime xs config t = interpreteMot config' (xs !! enieme)
  where enieme = round t `mod` 6
        config' = case config of
          config -> ((etatInitial config), (longueurPas config) * ((facteurEchelle config) ^ enieme), (facteurEchelle config), (angle config), (symbolesTortue config))

vonKoch1 :: LSysteme
vonKoch1 = lsysteme "F" regles
    where regles 'F' = "F-F++F-F"
          regles  s  = [s]

vonKoch2 :: LSysteme
vonKoch2 = lsysteme "F++F++F++" regles
    where regles 'F' = "F-F++F-F"
          regles  s  = [s]

hilbert :: LSysteme
hilbert = lsysteme "X" regles
    where regles 'X' = "+YF-XFX-FY+"
          regles 'Y' = "-XF+YFY+FX-"
          regles  s  = [s]

dragon :: LSysteme
dragon = lsysteme "FX" regles
    where regles 'X' = "X+YF+"
          regles 'Y' = "-FX-Y"
          regles  s  = [s]

vonKoch1Anime :: Float -> Picture
vonKoch1Anime = lsystemeAnime vonKoch1 (((-400, 0), 0), 800, 1/3, pi/3, "F+-")

vonKoch2Anime :: Float -> Picture
vonKoch2Anime = lsystemeAnime vonKoch2 (((-400, -250), 0), 800, 1/3, pi/3, "F+-")

dragonAnime :: Float -> Picture
dragonAnime = lsystemeAnime dragon (((0, 0), 0), 50, 1, pi/2, "F+-")

hilbertAnime :: Float -> Picture
hilbertAnime = lsystemeAnime hilbert (((-400, -400), 0), 800, 1/2, pi/2, "F+-")


main :: IO()
main = animate (InWindow "L-système" (1000, 1000) (0, 0)) white hilbertAnime 
