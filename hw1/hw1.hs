-- ES 1.1
countInversions :: (Ord a) => [a] -> Int
countInversions [] = 0
countInversions (x:xs) = checkInversions x xs + countInversions xs
    where
        checkInversions n = foldl (\inv x-> if x < n then inv+1 else inv) 0

-- ES 1.2
inversions [] = []
inversions (x:xs) = inv x xs ++ inversions xs
    where
        inv _ [] = []
        inv n ys = filter (\(x,y)-> x>y) (map (\y->(n, y)) ys)

-- ES 1.3
{-
 - Conto le inversioni sapendo che se a destra ho un y più piccolo
 - rispetto la x a sinistra nella fase di merge, tutti gli elementi
 - tra x e y sono più piccoli di x e quindi inversioni.
 - Uso il parametro n per mantenere il conto delle inversioni.
 -}
inversionsMergeSort :: Ord a => [a] -> Int
inversionsMergeSort = snd . mergeInversions
    where
        mrg [] ys result n = (result ++ ys, n)
        mrg xs [] result n = (result ++ xs, n)
        mrg xs@(x:txs) ys@(y:tys) result n
            | x <= y = mrg txs ys (result ++ [x]) n
            | otherwise = mrg xs tys (result ++ [y]) (n + 1 + length txs)

        mergeInversions [] = ([], 0)
        mergeInversions [x] = ([x], 0)
        mergeInversions xs = (result, ln + rn + nInvers)
            where
                n = div (length xs) 2
                (ls, rs) = splitAt n xs
                (lSortS, ln) = mergeInversions ls
                (rSortS, rn) = mergeInversions rs
                (result, nInvers) = mrg lSortS rSortS [] 0

-- Es. 2.1
segmentiSommaS [] _ = [[]]
segmentiSommaS xs s = filter (\x->sum x == s) (allSegment xs)
    where
        build []= []
        build xs = build (init xs) ++ [xs]

        allSegment [] = []
        allSegment xs@(_:txs) = build xs ++ allSegment txs

-- Es 2.2
{-
 - Per utilizzare una soluzione con powerset diverso da quello
 - visto a lezione ho optato per le maschere binarie da applicare
 - alla lista in input.
 -}

{-
 - Genera tutte le stringhe binarie lunghe n.
 - Il risultato è ordinato da [1, 1, .., 1_n] a [0, 0, .., 0_n]
 - per avere una stampa finale dell'esercizio che vada da sinistra
 - verso destra.
-}
genBin 0 = [[]]
genBin n = trueFalse [1] n ++ trueFalse [0] n
    where
        trueFalse xs  n
            | n == 1 = [xs]
            | otherwise = trueFalse (xs ++ [1]) (n-1) ++ trueFalse (xs ++ [0]) (n-1)

{-
 - Genera il powerset della lista xs applicando le maschere binarie.
-}
powerSet xs = map (\ys->applyMask xs ys) (genBin $ length xs)
    where
        applyMask xs ys = map fst (filter (\(x, y)-> y /= 0) $ zip xs ys)

sottoListeSommaS xs s = filter (\ys->sum ys == s) (powerSet xs)

-- ES 3.1
applyL (f:fs) (x:xs) = f x : applyL fs xs
applyL _ _           = []

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f = applyL . applyL [f | _<-[0..]]

-- ES 3.2
mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr f = foldr(\x ys -> f x : ys) []

-- ES. 3.3
mapFoldl :: (a -> b) -> [a] -> [b]
mapFoldl f = foldl(\ys x -> ys ++ [f x]) []

-- ES 3.4
{- Verificando il tipo di map "map :: (a -> b) -> [a] -> [b]" il ritorno della funzione
 - è una lista di un tipo generico.
 - Il valore ritorno di foldr (foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b) è invece un
 - tipo più generico di quello di map essendo solamente `b` (stessa cosa per foldl).
 - Quindi non è possibile definire fold[lr] con map perchè quello che possiamo ritornare è
 - solamente un sottoinsieme dei valori di ritorno invece possibili con i due fold.
 -}

 -- ES 4.1
{- Ho seguito come reference la pagina https://www.andreaminini.org/matematica/matematica-discreta/partizioni-di-un-numero-intero
 - che riporta la formula p_k (n) = p_{k−1} (n-1)+p_k (n−k) ma
 - che non ritorna il conteggio corretto.
 - Giocandoci un pò ho capito che si tratta di contare le soluzioni
 - con k e quelle senza k quindi la versione corretta della formula è
 - p_k (n) = p_{k-1} (n) + p_k (n-k).
 - Ho verificato i risultati confrontando con https://oeis.org/A000041/b000041.txt
-}
part :: Int -> Integer
part n = countPartition n n
    where
        countPartition n k
            | n == 0 = 1
            | k == 0 = 0
            | n < 0 = 0
            | otherwise = countPartition n (k-1) + countPartition (n-k) k

-- ES 4.2
{- Considerando anche le partizioni con l'ordine delle cifre in posizioni diverse
 - sono in tutto 2^n.
 -}

 -- ES 4.3
{-
 - Avendo utilizzato come reference principale la pagina wiki https://it.wikipedia.org/wiki/Partizione_di_un_intero
 - ho fatto tutti i ragionamenti partendo dal numero in input per poi andare
 - verso il basso alla partizione [1, 1...].
 - L'idea di partenza è stata quella tabellare della programmazione dinamica.
 -}

{-
- Dato i primo elemento e n la somma degli elementi costruisce la lista [i, i_1 ... i_k]
- costruisce la lista di partenza da cui calcolare le partizioni con i come testa.
- e.g. con i=3 n=8 restituisce [3, 3, 2]
-}
buildInitRow i n = i:buildRow i (n-i)
    where
        buildRow s n
            | n == 0    = []
            | s > n     = n:[]
            | otherwise = s:buildRow s (n-s)

{-
 - Controlla se la lista è nella forma [k, 1, 1 ..]
 - (in tal caso si dovrà passare alla lista [k-1, ..]).
-}
isLast [] = True
isLast (x:xs) = foldr (&&) True (map (\x -> x == 1 || x == 0 ) xs)

{-
 - Data xs partizione e i posizione di un elemento, sostituisce l'elemento con una sua partizione.
 - I partizionamenti vengono fatti partendo dalla coda andando verso la testa.
 -}
buildPart xs i
    | el == 1               = buildPart xs (i-1)
-- Se l'elemento è > 2 e la coda composta da tutti 1 ribilancio: e.g [4,3,1,1,1] -> [4, 2, 2, 2]
    | el > 2
      && (length xs)-1 /= i
      && succ == 1          = take i xs ++ buildInitRow (el-1) (sum (drop i xs))
    | el /= 1               = take i xs ++ (el - 1) : drop (i+1) xs ++ [1]
    where
        el = xs!!i
        succ = xs!!(i+1)

{-
 - Data una partizione produce tutte le possibili partizioni con l'elemento
 - in testa fissato.
 -}
buildParts xs = [xs] ++ next
    where
        result = buildPart xs ((length xs)-1)
        next = if isLast result then [result] else buildParts result

initPart 1 n = [buildInitRow 1 n]
initPart k n
    | k == 0 = []
-- La testa costruisce anche la riga successiva: [n] e [(n-1), 1]
    | k == n    = buildParts (buildInitRow n n) ++ initPart (k-2) n
    | otherwise = buildParts (buildInitRow k n) ++ initPart (k-1) n

parts :: Int -> [[Int]]
parts n = initPart n n

-- ES 4.4
part' :: Int -> Integer
part' xs = foldr (\_ n-> n+1) 0 $ parts xs

{- Per l'esercizio 4.3 parts i costi delle funzioni sono:
 - buildInit O(n)
 - isLast O(n)
 - buildParts O(n*n), n per lo scorrimento dell'array ed n per il 'ribilanciamento' della lista
 - initPart e buildParts invece se consideriamo |parts n| = k come il numero delle partizioni
 -     possibili per n, vengono eseguite k volte.
 - Non considerando il costo di (++) per concatenare le soluzioni il costo di parts è O(k * n^2)
 -
 - La soluzione di 4.1 part tramite formula invece ha costo esponenziale
 - perchè tutte le diramazioni dell'albero delle chiamate portano anche a soluzioni che
 - non sono corrette e che vengono scartate.
 -
 - La complessità di tempo di part' sarebbe quindi migliore rispetto a part dell'es. 4.1
 - (si aggiunge un +O(k) per la lunghezza che potrebbe essere spinto dentro la funzione e
 - calcolato tramite un parametro che viene aggiornato per ogni soluzione trovata).
 - Però se consideriamo le fusioni delle liste con ++, solamente in initPart
 - per ogni risultato aggiunto scomponiamo ogni volta quello già calcolato, aggiungendo
 - un fattore esponenziale.
 - La complessità di tempo di part' è quindi esponenziale.
 -}
