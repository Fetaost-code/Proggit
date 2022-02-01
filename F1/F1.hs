-- 1 och 4 skriven av Kei Duke Bergman. 
-- 3 Skriven av Isak Karlander. 
-- 2 skriven av båda.
module F1 where
import Data.Char
import qualified Data.Sequence as Seq

-- 1. Fibonacci-talen ↴
fib :: Int -> Integer
fib n = fibs !! n
    where
        fibs = 0 : 1 : fibs' 0 1
        fibs' a  b = (a + b) : fibs' b (a + b)
{-
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib' n 0 1 
    where
        fib' 1 a b  = b
        fib' n a b = fib' (n-1) b (a+b)
-}

-- 2. Rövarspråket ↴
-- Dessa konstanter används för att snabbt kolla om en char är en konsonant 
-- eller en rövarspråks variant av en konsonant
consonant = "bcdfghjklmnpqrstvwxz"

rovarsprak :: String -> String
rovarsprak (a:b)
    | a `elem` consonant    = a : 'o' : a : rovarsprak b
    | otherwise             = a : rovarsprak b
rovarsprak ""  = ""

karpsravor :: String -> String
karpsravor (a:b)
    |   a `elem` consonant    =  a : karpsravor (drop 2 b)
    |   otherwise             =  a : karpsravor b
karpsravor "" = ""                           

-- 3. Medellängd ↴
medellangd :: String -> Double 
medellangd text =
    fst sum / snd sum
    where
        sum = medellangd' text 0 0
        medellangd' (char:"") lc wc 
            | isAlpha char  = ((lc+1),(wc+1))
            | otherwise     = (lc,wc)
        medellangd' (char1:char2:text) lc wc 
            | isAlpha char1 && not (isAlpha char2)  = medellangd' (char2:text) (lc+1) (wc+1)
            | isAlpha char1                         = medellangd' (char2:text) (lc+1) wc
            | otherwise                             = medellangd' (char2:text) lc wc

{-
medellangd :: String -> Double
medellangd s = medellangd' s 0 0 False
    where
        medellangd' (a:b) totalLetters words formerWasAlpha
            |   isAlpha a       = medellangd' b (totalLetters + 1) words True
            |   formerWasAlpha  = medellangd' b totalLetters (words + 1) False
            |   otherwise       = medellangd' b totalLetters words False
        medellangd' [] 0 0 _ = 0
        medellangd' [] totalLetters words True = totalLetters/(words+1)
        medellangd' [] totalLetters words False = totalLetters/words
-}

-- 4. Listskyffling ↴
skyffla :: [a] -> [a]
skyffla l = smocka l [] 
smocka (a:b:l) evens    = a : smocka l (b:evens)
smocka [a] evens        = a : blocka evens 
smocka [] evens         = blocka evens 
blocka [] = []
blocka evens            = smocka (reverse evens) []


{-
skyffla :: [a] -> [a]
skyffla lista = skyffling [] lista 

skyffling :: [a] -> [a] -> [a]
skyffling skyfflat []           = skyfflat
skyffling skyfflat oskyfflat    = skyffling (skyfflat ++ evens oskyfflat) (odds oskyfflat)

odds :: [a] -> [a]
odds(head:tail) = evens(tail)

evens :: [a] -> [a]
evens [] = []
evens (head:[]) = [head]
evens (head:skip:tail) = (head:evens tail)
-}









