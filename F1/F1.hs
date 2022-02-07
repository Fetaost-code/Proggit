-- 1 och 4 skriven av Kei Duke Bergman. 
-- 3 Skriven av Isak Karlander. 
-- 2 skriven av båda.
module F1 where
import Data.Char

-- 1. Fibonacci-talen ↴
-- fib tar ett input n och returnerar värdet av det n:te Fibonaccitalet
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
-- consonant används för att snabbt evaluera om en char är en konsonant
consonant :: String
consonant = "bcdfghjklmnpqrstvwxz"

-- rovarsprak tar en String och gör om den till rövarspråk
rovarsprak :: String -> String
rovarsprak (a:b)
    | a `elem` consonant    = a : 'o' : a : rovarsprak b
    | otherwise             = a : rovarsprak b
rovarsprak ""  = "" -- avslutas om hela Stringen har utvärderats

-- karpsravor tar en String och gör den till icke-rövarspråk
karpsravor :: String -> String
karpsravor (a:b)
    |   a `elem` consonant    =  a : karpsravor (drop 2 b) -- droppar 'o':a
    |   otherwise             =  a : karpsravor b
karpsravor "" = "" -- avslutas om hela Stringen har utvärderats                     

-- 3. Medellängd ↴
-- medellangd tar en string och returnar den genomsnittliga mängden bokstavskaraktärer per ord
medellangd :: String -> Double 
medellangd text =
    fst sum / snd sum
    where
        sum = medellangd' text 0 0
        -- medellangd' är en stödfunktion som räknar antalet bokstäver och antalet ord i en String och returnar en tuple. Ordantalet ökar då ett ord avslutas.
        medellangd' (char:"") lc wc -- den sista karaktären utvärderas
            | isAlpha char  = ((lc+1),(wc+1)) -- är det en bokstav ökar bokstavsantalet och ordantalet med 1 innan det returnas
            | otherwise     = (lc,wc)
        medellangd' (char1:char2:text) lc wc 
            | isAlpha char1 && not (isAlpha char2)  = medellangd' (char2:text) (lc+1) (wc+1) -- Ett ord avslutas
            | isAlpha char1                         = medellangd' (char2:text) (lc+1) wc -- Ett ord pågår
            | otherwise                             = medellangd' (char2:text) lc wc -- Inte ett ord

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
-- skyffla kastar om en lista genom att ta ut vartannat tal och lägga in i en ny lista, och därefter utföra samma operation på övriga 
-- element tills att hela listan kastats om till en ny lista
skyffla :: [a] -> [a]
skyffla l = smocka l [] 

-- smocka
smocka :: [a] -> [a] -> [a]
smocka (a:b:l) evens    = a : smocka l (b:evens)
smocka [a] evens        = a : blocka evens 
smocka [] evens         = blocka evens 

-- blocka 
blocka :: [a] -> [a]
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









