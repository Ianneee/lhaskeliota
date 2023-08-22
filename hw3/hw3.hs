-- Es.1
insonnia = concat [show x ++ " sheep " | x <- [1..]]

-- Es.2
-- Genero il triangolo di tartaglia sapendo che ogni elemento della
-- riga n+1 è la somma dei due elementi adiacenti della riga n.
-- Aggiungo gli zeri perchè i due elementi alle estremità del triangolo
-- sono generati dall'unico elemento al di sopra, ovvero 1 per entrambi.
tartaglia = [1]:genT [1]
    where
        genT xs = nxs : genT nxs
            where
                nxs = zipWith (+) (xs ++ [0]) (0:xs)

-- Es.3
-- Comincio partendo dalla lista dei dispari.
-- La funzione gF riceve la lista dei numeri filtrati al passo precedente e
-- la posizione dell'elemento sopravvissuto da utilizzare per filtrare
-- la lista in quel giro, da quell'elemento in poi.
-- So che la testa della lista è un numero fortunato, lo aggiungo quindi tra
-- i risultati.
fortunati = 1:gF 2 [3,5..]
    where
        gF i (x:xs) = x:gF (i+1) ys
            where
                ys = [n | (n,i)<-zip xs [i+1..], i `mod` x /= 0]

-- Es.4
data BinTree a = Node a (BinTree a) (BinTree a) | Empty
    deriving (Show)

-- Es. 4.1
calkinWilf = buildCW (1, 1)
    where
        buildCW (m, n) = Node (m, n) (buildCW (m, m+n)) (buildCW (m+n, n))

-- Es. 4.2
-- Per i prossimi esercizi ho considerato alberi infiniti e completi non
-- avendo messo tutte le guardie necessarie per tali casi.
takeNlevels 0 _ = Empty
takeNlevels h (Node a ln rn) = Node a (takeNlevels (h-1) ln) (takeNlevels (h-1) rn)

-- Es. 4.3
-- Per fare la visita a livelli utilizzo una coda fifo. Estraggo la testa della coda
-- e accodo nodo sinistro e destro finchè non è vuota.
-- Ho bisogno della funzione enqueue per gestire il caso delle foglie.
enqueue xs (Node _ Empty Empty) = xs
enqueue xs (Node _ l r) = xs ++ [l, r]

visitaLivelli :: BinTree a -> [a]
visitaLivelli t = visit [t]
    where
        visit [] = []
        visit (x:xs) = a:visit ys
            where
                Node a _ _ = x
                ys = enqueue xs x

