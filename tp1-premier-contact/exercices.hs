module Exercices where

import Test.QuickCheck

-- exercices.hs
-- Author : Riad SABIR 


-- Exercice 3 : SommeDeXaY
-- signature:
sommeDeXaY :: Int -> Int -> Int
sommeDeXaYrecursive :: Int -> Int -> Int

--functions
sommeDeXaY x y = sum[x..y]

-- recursive version
sommeDeXaYrecursive x y = if y < x
                            then 0
                            else x + sommeDeXaYrecursive (x+1) y 

prop_sommeDeXaY x y = sommeDeXaYrecursive x y == sum [x..y]
-- in GHCi : "Prelude> quickCheck prop_sommeDeXaY"





-- Exercice 4 : Somme
-- signature
somme :: [Int] -> Int 
somme [] = 0
somme(x:xs) = if xs == [] 
                then x 
                else x + somme xs

prop_somme xs  = somme xs == sum xs
-- in GHCi : "Prelude> quickCheck prop_somme"



-- Exercice 5 : Last et Init
--Version 1
mlast :: [a] -> a --Renvoie le dernier element d'une liste (identique a last)
mlast (x:xs) = (x:xs)!!(length (x:xs)-1) -- On prend l'element en derniere position 

prop_mlast (x:xs) = mlast (x:xs) == last (x:xs)


--Version 2
mlast' :: [a] -> a
mlast' (x:xs) = if null(tail(x:xs)) --On teste recursivement si la queue est vide
                then x
                else mlast'(tail(x:xs))

prop_mlast' (x:xs) = mlast' (x:xs) == last (x:xs)


--Version 3
mlast'' :: [a] -> a
mlast'' (x:xs) = head(reverse((x:xs))) --On inverse la liste et on prend l'element en tete de liste

prop_mlast'' (x:xs) = mlast'' (x:xs) == last (x:xs)


--Version 4
mlast''' :: [a] -> a
mlast''' (x:xs) = head (drop (length(x:xs)-1) (x:xs)) --On prend la tete de la liste composée uniquement du dernier element

prop_mlast''' (x:xs) = (mlast''' (x:xs)) == (last (x:xs))


-- Version 5
my_last :: [a] -> a  
my_last xs = if length xs == 0 then error "Empty list" else xs !! (length xs - 1)
my_last' xs = head (reverse xs)

prop_my_last xs = my_last xs == last xs
-- in GHCi : "Prelude> quickCheck prop_my_last"


--Version 1
minit :: [a] -> [a]
minit (x:xs) = if null(xs) --Filtrage a gauche tant qu'il reste plus d'un element (le dernier renvoie une liste vide)
               then []
               else [x] ++ minit(xs)

prop_minit (x:xs) = minit (x:xs) == init (x:xs)


--Version 2
minit' :: [a] -> [a]
minit' (x:xs) = take (length(x:xs)-1) (x:xs) --On prend tous les elements sauf le dernier

prop_minit' (x:xs) = minit' (x:xs) == init (x:xs)



--Exercice 6 : !! ++ concat map
-- !!
double_exclamation n (x:xs) = if n == 0 --Filtrage a gauche
                              then x
                              else double_exclamation (n-1) (xs)


-- ++
double_plus :: [a] -> [a] -> [a]
double_plus xs [] = xs
double_plus [] ys = ys 
double_plus (x:[]) ys = x:ys
double_plus (x:xs) (y:ys) = x:double_plus xs (y:ys)  


-- concat
concat' :: [[a]] -> [a] --On concatene chaque sous liste
concat' [[]] = []
concat' (x:xs) = x ++ (concat xs)

prop_concat' (x:xs) = concat' (x:xs) == concat (x:xs)
prop_double_plus xs ys = double_plus xs ys == xs ++ ys 


-- map
map' :: (a->a) -> [a] -> [a]
map' f (x:xs) = if null(xs) -- On créé une nouvelle liste en appliquant sur chaque élément la fonction 
                then [f(x)]
                else [f(x)] ++ map f (xs)

prop_map' f (x:xs) = map' f (x:xs) == map f (x:xs)




--Exercice 7 : La déclaration x = (!!) l indique que l'on applique l'argument x à la méthode !! ,la liste l étant un argument prédéfini.
x :: Int -> Int
x = (!!) [1,2,3]



--Exercice 8 : Longueur liste avec somme et map 
longueur_liste :: [a] -> Int
longueur_liste xs = case xs of
                      [] -> 0 
                      x:xs' ->  sum(map (\n -> 1) (x:xs') )

prop_longueur_liste xs = longueur_liste xs == length xs

--Exercice 9 : Construction d'une liste composée d'iteration de fonction
fonction_itere_rec f x n = take n (iterate (f) x) -- On prend les n elements de la liste générée avec iterate

fonction_itere_standard f x n | n <= 0 = [] --Si aucun element a creer
fonction_itere_standard f x n | n>0 = [x] ++ fonction_itere_standard f (f(x)) (n-1) --Sinon on applique la fonction à l'élement ainsi qu'au prochain element

--Exercice 10 : Somme de 0 à n
sommeDe0aN :: Int -> [Int]
sommeDe0aN n = fonction_itere_standard (+1) 0 (n+1)

prop_sommeDe0aN n = (take (n+1) (iterate (+1) 0)) == (sommeDe0aN n)

