module Arbres where

-- TP4: Des arbres et des couleurs
--
-- Riad SABIR 


import Test.QuickCheck 
import Control.Concurrent (threadDelay)
import Data.Char
import Data.List

-- Question 1

data Arbre color a  = Noeud { coul :: color,
                       val :: a,
                       gauche :: Arbre color a,
                       droit :: Arbre color a
                      } | Feuille
                      deriving (Show,Eq)

-- Question 2
mapArbre :: (c -> m) -> (a -> b) -> Arbre c a  -> Arbre m b
mapArbre _ _ Feuille = Feuille
mapArbre f g noeud = Noeud (f (coul noeud))  (g (val noeud)) (mapArbre f g (gauche noeud)) (mapArbre f g (droit noeud))


-- Test tree
myTree = Noeud { coul = "",
                 val = 5,
                 gauche = Noeud { coul = "",
                                 val = 4,
                                 gauche = Feuille,
                                 droit = Feuille},
                 droit = Noeud { coul = "",
                                 val = 9,
                                 gauche = Feuille,
                                 droit = Feuille }
              }


-- Question 3
hauteur :: Arbre c a -> Int
hauteur Feuille = 0
hauteur t = 1 + max (hauteur (gauche t) ) (hauteur (droit t))

taille :: Arbre c a -> Int
taille Feuille = 0
taille t = 1 + taille (gauche t) + taille (droit t)

-- Question 4

dimension :: (Int -> Int -> Int) -> Arbre c a -> Int
dimension _ Feuille = 0
dimension f t = 1 + f (dimension f (gauche t)) (dimension f (droit t))

-- Question 5

peigneGauche :: [(c,a)] -> Arbre c a
peigneGauche [] = Feuille
peigneGauche (x:xs) = Noeud (fst x) (snd x) (peigneGauche xs) (Feuille)

-- Question 6

-- Test
prop_hauteurPeigne xs = length xs == hauteur (peigneGauche xs)


-- Question 7
prop_taille xs = length xs == taille ( peigneGauche xs)
prop_dimensionHeight xs = length xs == dimension max (peigneGauche xs)
prop_dimensionTaille xs = length xs == dimension (+) (peigneGauche xs)
-- test mapArbre

-- Question 8
estComplet :: Arbre c a -> Bool
estComplet Feuille = True
estComplet t = if taille (gauche t) == taille (droit t)
                        then estComplet (gauche t) && estComplet (droit t)
                        else False

-- Test
prop_estComplet xs = estComplet (peigneGauche xs)


-- Question 9

-- Question 10



-- Question 11
complet :: Int -> [(c, a)] -> Arbre c a
complet 0 [] = Feuille
complet 1 (x:[]) = Noeud {
                          coul   = fst(x), 
                          val    = snd(x), 
                          gauche = Feuille, 
                          droit  = Feuille
                         }


complet 2 (x:y:z:xs) = Noeud {
                              coul   = fst(y), 
                              val    = snd(y),
                              gauche = complet (1) (x:xs),
                              droit  = complet (1) (z:xs)
                             }

complet n xs = Noeud (color) (value) (leftTree) (rightTree)
                      where            
                      color     = fst(xs !! (quot ((length xs) - 1) 2))
                      value     = snd(xs !! (quot ((length xs) - 1) 2))
                      leftTree  = complet (n-1) (take (quot ((length xs) - 1) 2) xs)
                      rightTree = complet (n-1) (drop (quot ((length xs) + 1) 2) xs)



{-
Color and Value = Root
Left Tree = first half of the list (without the root)
Right Tree = second half of the list (without the root)
-}

-- Question 12
myRepeat :: a -> [a]
myRepeat = iterate $ id 

-- Question 13
allCharacter :: [Char]
allCharacter = iterate (\x -> (toEnum(fromEnum (x) + 1))) 'a'
valuesToNodes :: [((),Char)]
valuesToNodes = map (\x -> ((),x)) allCharacter --Create an infinite list of tuples who contains [('()', 'ordered character')]

-- Question 14
aplatit :: Arbre c a -> [(c, a)]
aplatit Feuille = []
aplatit arbre = (aplatit (gauche arbre)) ++ [(coul arbre, val arbre)] ++ (aplatit (droit arbre))

-- Test
complet1 = complet 1 (take 1 valuesToNodes)
complet2 = complet 2 (take 3 valuesToNodes)
complet3 = complet 3 (take 7 valuesToNodes)
complet4 = complet 4 (take 15 valuesToNodes)
complet5 = complet 5 (take 31 valuesToNodes)
prop_aplatit'  = map snd (aplatit complet4) == "abcdefghijklmno"
prop_aplatit n = map snd (aplatit (complet n (take ((2^n)-1) valuesToNodes))) == take ((2^n)-1) allCharacter

-- Question 15
element :: Eq a => a -> Arbre c a -> Bool
element e arbre = elem e (map snd(aplatit arbre))

-- Question 16

colorToString :: Show c => c -> String
colorToString color = "[color=" ++ (show color) ++ ", fontcolor=" ++ (show color) ++ "]"

valueToString :: (Show a) => a -> String
valueToString value = removeQuote(show value)

removeQuote :: String -> String
removeQuote word = filter (not . (`elem` "\'")) word

noeud :: (c -> String) -> (a -> String) -> (c,a) -> String
noeud fcolor fvalue (c,a) = (fvalue a) ++ (fcolor c) 

-- Question 17
arcs :: Arbre c a -> [(a,a)]
arcs (Noeud _ _ Feuille Feuille) = []
arcs (Noeud _ a Feuille fd ) = [(a, val fd)] ++ (arcs (fd))
arcs (Noeud _ a fg Feuille ) = [(a, val fg)] ++ (arcs (fg))
arcs arbre = [(val arbre, val (gauche arbre)),(val arbre, val (droit arbre))] ++ (arcs (gauche arbre)) ++ (arcs (droit arbre))

-- Question 18
arc :: (a -> String) -> (a,a) -> String
arc fvalue (pere,fils) = (fvalue pere) ++ " -> " ++ (fvalue fils)

-- Question 19
dotise :: String -> (c -> String) -> (a -> String) -> Arbre c a -> String
dotise name fcolor fvalue arbre = "/* Entete */\n"
                              ++ "digraph " ++ show name ++ " {"
                              ++ "\nnode [fontname=\"DejaVu-Sans\", shape=circle] \n\n"
                              ++  " /* Liste des noeuds */\n"
                              ++  unlines (map (noeud fcolor fvalue) (aplatit arbre))
                              ++  " /* Liste des arcs */\n"
                              ++  unlines (map (arc fvalue) (arcs arbre))
                              ++  "}"
     
                       
-- Question 20

elementR :: Ord a => a -> Arbre color a -> Bool
elementR _ Feuille = False
elementR e arbre | e < val arbre  = elementR e (gauche arbre)
                 | e > val arbre  = elementR e (droit arbre)
                 | otherwise      = True

-- Question 21

data Couleur = Red | Black deriving (Eq,Show) --Equality, because when we use the function equilibre, we check the color of the node

type ArbreRN a = Arbre Couleur a  -- coul field is now a Couleur Type

                      
-- Question 22
equilibre :: Eq a => ArbreRN a -> ArbreRN a
equilibre arbre 
                |  arbre == Feuille = Feuille --Base case

                |  gauche arbre /= Feuille &&
                   coul (gauche arbre) == Red &&
                   gauche (gauche arbre) /= Feuille && 
                   coul (gauche(gauche arbre)) == Red = Noeud Red fgDevientRacine fgfgDevientfg racineDevientfd
                                         

                | gauche arbre /= Feuille &&
                  coul (gauche arbre) == Red &&
                  droit (gauche arbre) /= Feuille  && 
                  coul (droit(gauche arbre)) == Red   = Noeud Red fgfdDevientRacine fgRestefg racineDevientfdd

                | droit arbre /= Feuille &&
                  coul (droit arbre) == Red &&
                  gauche (droit arbre) /= Feuille  && 
                  coul (gauche(droit arbre)) == Red  =  Noeud Red fdfgDevientRacine racineDevientfg fdRestefd

                | droit arbre /= Feuille &&
                  coul (droit arbre) == Red &&
                  droit (droit arbre) /= Feuille  && 
                  coul (droit(droit arbre)) == Red  =  Noeud Red fdDevientRacine racineDevientfgg fdfdDevientfd

                | otherwise = Noeud (coul arbre) (val arbre) (equilibre (gauche arbre)) (equilibre (droit arbre)) --Recursive call

                                          where 

                                                -- LeftLeftTree
                                                fgDevientRacine   = val    (gauche arbre)
                                                fgfgDevientfg     = Noeud Black 
                                                                        (val(gauche(gauche arbre)))
                                                                        (gauche (gauche (gauche arbre)))
                                                                        (droit  (gauche (gauche arbre)))
                                                racineDevientfd = Noeud Black 
                                                                        (val arbre)
                                                                        (droit (gauche arbre))
                                                                        (droit arbre)

                                                -- LeftRightTree
                                                fgfdDevientRacine = val (droit(gauche arbre))
                                                fgRestefg       = Noeud Black 
                                                                        (val(gauche arbre))
                                                                        (gauche (gauche arbre))
                                                                        (gauche  (droit (gauche arbre)))
                                                racineDevientfdd = Noeud Black 
                                                                        (val arbre)
                                                                        (droit(droit (gauche arbre)))
                                                                        (droit arbre)

                                                -- RightLeftTree
                                                fdfgDevientRacine = val (gauche(droit arbre))
                                                
                                                racineDevientfg = Noeud Black 
                                                                        (val arbre)
                                                                        (gauche arbre)
                                                                        (gauche(gauche(droit arbre)))

                                                fdRestefd       = Noeud Black 
                                                                        (val(droit arbre))
                                                                        (droit(gauche (droit arbre)))
                                                                        (droit(droit arbre))

                                                -- RightRightTree
                                                fdDevientRacine  = val (droit arbre)
                                                
                                                racineDevientfgg = Noeud Black 
                                                                        (val arbre)
                                                                        (gauche arbre)
                                                                        (gauche(droit arbre))

                                                fdfdDevientfd    = Noeud Black 
                                                                        (val(droit(droit arbre)))
                                                                        (gauche(droit (droit arbre)))
                                                                        (droit(droit(droit arbre)))

-- Test tree LeftLeft
myLeftLeftColoredTree = Noeud { coul   = Black,
                                val    = 'z',
                                gauche = Noeud { coul   = Red,
                                                 val    = 'y',
                                                 gauche = Noeud { coul   = Red,
                                                                  val    = 'x',
                                                                  gauche = Feuille,
                                                                  droit  = Feuille},
                                                 droit  = Feuille},
                                droit  = Feuille
              }

-- Test tree LeftRight
myLeftRightColoredTree = Noeud { coul   = Black,
                                val    = 'z',
                                gauche = Noeud { coul   = Red,
                                                 val    = 'x',
                                                 gauche = Feuille,
                                                 droit  = Noeud { coul   = Red,
                                                                  val    = 'y',
                                                                  gauche = Feuille,
                                                                  droit  = Feuille}},
                                droit  = Feuille
              }

-- Test tree RightLeft
myRightLeftColoredTree = Noeud { coul   = Black,
                                val    = 'x',
                                gauche = Feuille,
                                droit  = Noeud { coul   = Red,
                                                 val    = 'z',
                                                 gauche = Noeud { coul   = Red,
                                                                  val    = 'y',
                                                                  gauche = Feuille,
                                                                  droit  = Feuille},
                                                 droit  = Feuille}
              }

-- Test tree RightRight
myRightRightColoredTree = Noeud { coul   = Black,
                                val    = 'x',
                                gauche = Feuille,
                                droit  = Noeud { coul   = Red,
                                                 val    = 'y',
                                                 gauche = Feuille,
                                                 droit  = Noeud { coul   = Red,
                                                                  val    = 'z',
                                                                  gauche = Feuille,
                                                                  droit  = Feuille}}
              }


-- Question 23
blackRoot :: (Eq a,Ord a) => ArbreRN a -> ArbreRN a
blackRoot Feuille         = Feuille
blackRoot (Noeud _ x g d) = Noeud Black x g d


insertionArbreRN :: (Eq a,Ord a)  => ArbreRN a -> a -> ArbreRN a
insertionArbreRN arbre valeur = blackRoot (ins valeur arbre)
  where ins v Feuille                              = Noeud Red v Feuille Feuille
        ins v abr@(Noeud c r g d) | elementR v abr = abr
                                  | v < r          = equilibre (Noeud c r (ins v g) d)
                                  | otherwise      = equilibre (Noeud c r g (ins v d))

-- Question 24

-- Question 25
arbresDot :: (Show a, Ord a) => [a] -> [String]
arbresDot values = map (dotise "arbre" colorToString valueToString) treeList
                   where treeList = unfoldr (\(a,b) -> if (null b) 
                                                       then Nothing 
                                                       else Just (insertionArbreRN a (head b),(insertionArbreRN a (head b), (tail b)))) (Feuille, values) 


main = mapM_ ecrit arbres
    where ecrit a = do writeFile "arbre.dot" a
                       threadDelay 1000000
          arbres  = arbresDot "gcfxieqzrujlmdoywnbakhpvst"

