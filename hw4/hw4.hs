import Control.Monad.ST
import Data.Array.ST

-- Esercizio 1
-- Nello svolgimento considero solo le lettere minuscole convertendo le
-- maiuscole, scarto tutto quello che non è una lettera.
-- Per contare le lettere utilizzo gli array mutabili con la monade ST
-- visti nel quick sort.

charCount :: IO ()
charCount = do putStr "Insert the number of strings you want to insert:\n> "
               c <- atoi <$> getLine
               case c of
                   Nothing  -> putStrLn "Input Error: Insert a valid number!"
                   Just val -> do putStrLn $ "Insert " ++ show val ++ " strings:"
                                  inputs <- getInputs val
                                  putStrLn $ "Letters count:"
                                  prettyPrint $ doCount $ clean inputs

-- Converto il char ricevuta nel corrispettivo intero verificando
-- che si tratti effettivamente di una cifra.
cToDigit :: Char -> Maybe Int
cToDigit x
    | c < 0 || c > 9 = Nothing
    | otherwise      = Just c
    where
        c = fromEnum x - fromEnum '0'

-- Implementazione di atoi con verifica degli errori (errori: lettere invece di cifre)
atoi :: String -> Maybe Int
atoi xs = atoiAux xs 0
    where
        atoiAux [x] n    = do y <- cToDigit x
                              return (n + y)

        atoiAux (x:xs) n = do y <- cToDigit x
                              atoiAux xs ((n + y) * 10)

-- Funzione per ricevere le stringhe inserite dall'utente
getInputs :: Int -> IO [String]
getInputs 0 = return []
getInputs n = do putStr "> "
                 x  <- getLine
                 xs <- getInputs (n-1)
                 return (x:xs)

-- Per ogni stringa converto tutte le lettere in minuscolo, le ordino Per
-- rimuovere i duplicati, e poi concateno il tutto.
clean :: [String] -> [Maybe Char]
clean = concat . map (mergeSort . map toLower)

-- Converto le lettere maiuscole in minuscole e scarto tutto il resto
-- tipo spazi ed altri segni.
toLower :: Char -> Maybe Char
toLower c
    | k <= z  && k >= a  = return $ toEnum k
    | k <= zU && k >= aU = return $ toEnum (a + (k - aU))
    | otherwise = Nothing
    where
        k  = fromEnum c
        z  = fromEnum 'z'
        a  = fromEnum 'a'
        aU = fromEnum 'A'
        zU = fromEnum 'Z'

-- Riordino eliminando i duplicati e i valori Nothing
mergeSort :: Ord a => [Maybe a] -> [Maybe a]
mergeSort []  = []
mergeSort [x] = [x]
mergeSort xs  = merge (mergeSort ls) (mergeSort rs)
    where
        n = length xs `div` 2
        (ls, rs) = splitAt n xs

        merge [] ys = ys
        merge xs [] = xs
        merge xs@(x:txs) ys@(y:tys)
            | x == Nothing = merge txs ys
            | y == Nothing = merge xs tys
            | x == y       = merge txs ys
            | x < y        = x:merge txs ys
            | otherwise    = y:merge xs tys

-- Chiamo la funzione che esegue il conteggio e zippo il risultato ottenuto
-- con le lettere corrispondenti (il risultato del conteggio è ordinato)
doCount :: [Maybe Char] -> [(Char, Int)]
doCount xs = zip ['a'..'z'] (runST $ doCountAux xs)

-- Per il conteggio uso un array mutabile in cui gli indici sono le lettere
-- ed i valori mantengono il conteggio
doCountAux :: [Maybe Char] -> ST s [Int]
doCountAux xs = do xa <- newArray ('a', 'z') 0 :: ST s (STArray s Char Int)
                   countLetters xs xa
                   getElems xa

-- Scorro tra le lettere ed incremento il corrispettivo conteggio
countLetters :: [Maybe Char] -> STArray s Char Int -> ST s ()
countLetters [] xa = return ()
countLetters (x:xs) xa = do let (Just l) = x
                            val <- readArray xa l
                            writeArray xa l (val+1)
                            countLetters xs xa

prettyPrint :: [(Char, Int)] -> IO ()
prettyPrint [] = return ()
prettyPrint ((l, q):xs) = do putStrLn $ l : ": " ++ show q
                             prettyPrint xs


-- Esercizio 2, le funzioni che calcolano se il numero è
-- un semi-perfetto sono le `isSemiPerf`

divisori :: Int -> [Int]
divisori x = [n | n <- [1..k], x `mod` n == 0]
    where
        k = x `div` 2

powerset :: [a] -> [[a]]
powerset []     = [[]]
powerset (x:xs) = (pure (x:) <*> powerset xs) ++ powerset xs

-- Con Maybe come richiesto dal testo
checkSemiPerf :: Int -> [Int] -> Maybe [Int]
checkSemiPerf x xs
    | sum xs == x = Just xs
    | otherwise   = Nothing

isSemiPerf :: Int -> Bool
isSemiPerf x = any (\xs -> (checkSemiPerf x xs /= Nothing)) $ powerset dividers
    where
        dividers = divisori x

-- Ma si può fare in maniera veloce anche senza Maybe, come era stato detto in aula
isSemiPerf' :: Int -> Bool
isSemiPerf' x = any (\xs -> sum xs == x) $ powerset $ divisori x

-- Non ho ben capito la definizione: se i semi perfetti sono un sottoinsieme
-- degli abbondanti o il contrario. Nel caso fossero un sottoinsieme degli abbondanti:
isSemiPerf'' :: Int -> Bool
isSemiPerf'' x = abbondante && semiPerfetto
    where
        dividers     = divisori x
        abbondante   = sum dividers > x
        semiPerfetto = any (\xs -> sum xs == x) $ powerset dividers


-- Esercizio 3 Monadi I
data MyEither a b = MyLeft b | MyRight a
    deriving (Show, Eq, Ord)

instance Functor (MyEither b) where
    fmap f (MyLeft l)  = MyLeft (f l)
    fmap _ (MyRight r) = MyRight r

instance Applicative (MyEither b) where
    pure = MyLeft
    MyRight r <*> _ = MyRight r
    MyLeft f  <*> x = fmap f x

instance Monad (MyEither b) where
    return          = MyLeft
    MyRight r >>= _ = MyRight r
    MyLeft l  >>= f = f l

-- Esercizio 3 Monadi II
safeDiv _ 0 = MyRight "Error: Division by zero"
safeDiv m n = MyLeft (m `div` n)

safeSub m n
    | m < n     = MyRight "Error: The minuend is smaller than the subtrahend"
    | otherwise = MyLeft (m - n)

data Term = Const Int | Add Term Term | Sub Term Term | Mult Term Term | Div Term Term

eval :: Term -> MyEither String Int
eval (Const a) = pure a

eval (Add t u) = do a <- eval t
                    b <- eval u
                    return (a + b)

eval (Sub t u) = do a <- eval t
                    b <- eval u
                    a `safeSub` b

eval (Mult t u) = do a <- eval t
                     b <- eval u
                     return (a * b)

eval (Div t u) = do a <- eval t
                    b <- eval u
                    a `safeDiv` b

-- Test vari
a = (Add (Add (Const 20)(Const 10))(Const 12))
s = (Sub (Sub (Const 82)(Const 20))(Const 20))
sError = (Sub (Sub (Const 82)(Const 20))(Const 142))
sError' = (Sub (Const 82)(Const 104))

m = (Mult (Const 21)(Const 2))
d = (Div (Div (Const 1972)(Const 2))(Const 23))
dZero = (Div (Div (Const 1972)(Const 2))(Const 0))

