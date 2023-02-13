-- Interprete

-- Riad SABIR 



module Main where

import Parser
import Data.Maybe
import System.IO
import Control.Monad
import Data.Either

type Nom = String

data Expression = Lam Nom Expression
                | App Expression Expression
                | Var Nom
                | Lit Litteral
                deriving (Show,Eq)

data Litteral = Entier Integer
              | Bool   Bool
              deriving (Show,Eq)


-- 1 Parseur qui consomme tout les éspaces
-- au début de la chaine analysé
espacesP :: Parser ()
espacesP = (many  (car ' ') ) >>= \_ -> pure ()


-- 2  Un analyseur qui analyse le premier nom dans une chaine
smallChar x = x `elem` ['a' .. 'z']

nomP :: Parser Nom
nomP = do nom <- some (carQuand smallChar)
          espacesP
          pure nom

-- 3 parsing an expression
varP :: Parser Expression
varP = do varName <- nomP
          pure (Var varName)



-- 4 Applique
applique' :: [Expression] -> Expression
applique' [e] = e
applique' [e1, e2] = App e1 e2
applique' (e1:e2:es) =  let h = applique' [e1, e2] in applique'(h:es)


-- Applique avec foldl
applique :: [Expression] -> Expression
applique = foldl1 (\x y -> App x y)


-- expression parsers
exprP :: Parser Expression
exprP = varP <|> lambdaP <|> exprParentheseeP <|> nombreP <|> booleenP

exprsP :: Parser Expression
exprsP = some exprP >>= \l -> pure $ applique l


-- Lambda Parser
lambdaP :: Parser Expression
lambdaP = do chaine "λ"
             nom <- nomP
             espacesP
             chaine "->"
             espacesP
             expression <- exprsP
             pure (Lam nom expression)


-- parantheses
exprParentheseeP :: Parser Expression
exprParentheseeP = do car '('
                      espacesP
                      expr <- exprsP
                      car ')'
                      espacesP
                      pure expr


-- nombres
nombreP :: Parser Expression
nombreP = do nb <- some $ carQuand (\x -> x `elem` ['0' .. '9'])
             espacesP
             pure (Lit ( Entier (read nb)))


booleenP :: Parser Expression
booleenP = chaine "True" <|> chaine "False"
           >>= \l -> espacesP >>= \_ -> case l of
                                            "True" -> pure (Lit(Bool True))
                                            "False" -> pure (Lit(Bool False))

expressionP :: Parser Expression
expressionP = do espacesP
                 e <- exprsP
                 espacesP
                 pure e


ras :: String -> Expression
ras s | Just(_ , "") <- parsed   = resultat parsed
      | otherwise                = error "Erreur d’analyse syntaxique"
      where parsed  = runParser expressionP s


data ValeurA = VLitteralA Litteral
             | VFonctionA (ValeurA -> ValeurA)

-- Show ne sait pas traiter les types ValeurA !!!
instance Show ValeurA where
    show (VFonctionA _)          = "λ"
    show (VLitteralA (Entier n)) = show n
    show (VLitteralA (Bool b))   = show b



type Environnement a = [(Nom, a)]


interpreteA :: Environnement ValeurA -> Expression -> ValeurA
interpreteA _   (Lit l)     = VLitteralA l
interpreteA env (Var x)     = fromJust (lookup x env)
interpreteA env (Lam n e)   = VFonctionA f
                where f v = interpreteA ((n,v) : env) e
interpreteA env (App e1 e2) = f (interpreteA env e2)
                where VFonctionA f = interpreteA env e1



negA :: ValeurA
negA = let f = \(VLitteralA (Entier n)) -> VLitteralA (Entier ((-1) * n)) in VFonctionA f



addA :: ValeurA
addA = VFonctionA (\(VLitteralA (Entier e)) -> VFonctionA (\(VLitteralA (Entier e')) -> VLitteralA (Entier (e + e'))))


releveBinOpEntierA :: (Integer -> Integer -> Integer) -> ValeurA
releveBinOpEntierA op = VFonctionA (\(VLitteralA (Entier e)) -> VFonctionA (\(VLitteralA (Entier e')) -> VLitteralA (Entier (op e e'))))




envA :: Environnement ValeurA
envA = [ ("neg",   negA)
       , ("add",   releveBinOpEntierA (+))
       , ("soust", releveBinOpEntierA (-))
       , ("mult",  releveBinOpEntierA (*))
       , ("quot",  releveBinOpEntierA quot) ]



ifthenelseA :: ValeurA
ifthenelseA = VFonctionA (\(VLitteralA (Bool bool)) -> VFonctionA (\(VLitteralA lit1) ->
                                                           VFonctionA (\(VLitteralA lit2) ->
                                                                           case bool of   True  -> (VLitteralA lit1)
                                                                                          False -> (VLitteralA lit2))))


first_prompt :: IO()
first_prompt = do putStr "minilang> "
                  hFlush stdout
                  b <- isEOF
                  unless b $ do l <- getLine
                                print ( interpreteA envA ( ras l ))
                                first_prompt


data ValeurB = VLitteralB Litteral
             | VFonctionB (ValeurB -> ErrValB)

type MsgErreur = String
type ErrValB   = Either MsgErreur ValeurB


instance Show ValeurB where
    show (VFonctionB _)          = "λ"
    show (VLitteralB (Entier n)) = show n
    show (VLitteralB (Bool b))   = show b



interpreteB :: Environnement ValeurB -> Expression -> ErrValB
interpreteB _   (Lit l)     = Right (VLitteralB l)
interpreteB env (Var x )    = case res of True  -> Right( fromJust ev )
                                          False -> Left ("la variable " ++ x ++ " n'est pas definie") -- modify here
                              where ev = lookup x env
                                    res = isJust ev

interpreteB env (Lam n e)   = Right(VFonctionB f)
                              where f v = interpreteB ((n,v) : env) e

interpreteB env (App e e')  = interp int_e int_e'
                              where int_e  = interpreteB env e
                                    int_e' = interpreteB env e'

                                    interp (Right(VFonctionB f )) (Right r ) = f r
                                    interp (Right(VFonctionB f )) (Left  r ) = Left r
                                    interp (Left m)                        _ = Left m
                                    interp x                               _ = Left (show x ++ " n'est pas une fonction, application impossible")


addB :: ValeurB
addB = VFonctionB f
       where f (VLitteralB(Entier n1)) = Right(VFonctionB h)
               where h (VLitteralB(Entier n2)) = Right(VLitteralB (Entier (n1 + n2) ))
                     h x                       = Left (show x ++ " n'est pas un entier")
             f x                       = Left (show x ++ "n'est pas un entier")





quotB :: ValeurB
quotB =  VFonctionB f
         where f (VLitteralB(Entier n1)) = Right(VFonctionB h)
                  where h (VLitteralB (Entier 0 )) = Left ("division par 0 ")
                        h (VLitteralB (Entier n2)) = Right (VLitteralB(Entier(quot n1 n2)))
                        h x                        = Left (show x ++ " n'est pas un entier ")
               f x                       = Left( show x ++ " n'est pas un entier")






data ValeurC = VLitteralC Litteral
             | VFonctionC (ValeurC -> OutValC)

type Trace   = String
type OutValC = (Trace, ValeurC)


--main :: IO()
--main = first_prompt
