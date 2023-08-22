{- Esercizio 1
 - Il costo computazione rimane O(nlogn): invece di ordinare parti destre
 - e sinistre sto ordinando tra vicini, il numero di passaggi necessari rimane
 - sostanzialmente lo stesso e cambia soltanto l'ordine in cui gruppi di
 - elementi vengono riordinati nella fase di merge.
 -}
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort xs = head (mergeAux (singletons xs) (fromIntegral $ length xs))
    where
        -- Lista liste lunghe 1
        singletons = map (\x-> [x])

        -- Ordina e "mergia" le liste interne finchè non ne rimane una sola
        mergeAux xxs len
            | len <= 1 = xxs
            | otherwise = mergeAux (unPack xxs) (len / 2)

        -- Prende i primi 2 elementi della lista per "mergiarli"
        unPack [] = []
        unPack [xs] = [xs]
        unPack (x:y:xs) = merge x y : unPack xs

        merge [] ys = ys
        merge xs [] = xs
        merge xs@(x:txs) ys@(y:tys)
            | x <= y = x:merge txs ys
            | otherwise = y:merge xs tys


mergeSortB :: Ord a => [a] -> [a]
mergeSortB [] = []
mergeSortB xs = (mergeAux (singletons xs))
    where
        -- Lista liste lunghe 1
        singletons = map (\x-> [x])

        -- Ordina e "mergia" le liste interne finchè non ne rimane una sola
        mergeAux :: [[a]] -> [a]
        mergeAux [xs] = xs
        mergeAux xxs@(x:xs) = mergeAux (unPack xxs)

        -- Prende i primi 2 elementi della lista per "mergiarli"
        unPack [] = []
        unPack [xs] = [xs]
        unPack (x:y:xs) = merge x y : unPack xs

        merge [] ys = ys
        merge xs [] = xs
        merge xs@(x:txs) ys@(y:tys)
            | x <= y = x:merge txs ys
            | otherwise = y:merge xs tys



-- Esercizio 2
data BinTree a = Node a (BinTree a) (BinTree a) | Empty
    deriving (Show)

mapBT :: (a->b) -> BinTree a -> BinTree b
mapBT _ Empty = Empty
mapBT f (Node a rn ln) = Node (f a) (mapBT f rn) (mapBT f ln)

foldrBT :: (b -> a -> b -> b) -> b -> BinTree a -> b
foldrBT _ v Empty = v
foldrBT f v (Node a ln rn) = f ls a rs
    where
        ls = foldrBT f v ln
        rs = foldrBT f v rn

-- 2.1
nodes = foldrBT (\x _ y -> x+1+y) 0

-- 2.2
height = foldrBT (\x _ y -> 1 + max x y) 0

-- 2.3
-- Sfrutto il tupling per passarmi l'altezza massima tra sotto albero dx e sx
-- e il paramentro per il massimo sbilanciamento trovato ad ogni passo.
umbalance = snd . foldrBT (umbAux) (0,0)
    where
        umbAux (lh, lu) _ (rh, ru) = (height, umb)
           where
               height = 1 + max lh rh
               -- Selgo il massimo tra l'umbalance da sottoalbero dx e sx e quello
               -- nuovo calcolato dalla differenza tra altezza sx e dx
               umb = max (max lu ru) (abs (lh - rh))

data BinTree' a = Node' (BinTree' a) (BinTree' a) | Leaf a
    deriving (Show)

mapBT' :: (a->b) -> BinTree' a -> BinTree' b
mapBT' f (Leaf a) = Leaf $ f a
mapBT' f (Node' ln rn) = Node' (mapBT' f ln) (mapBT' f rn)

foldrBT' :: (a -> a -> a) -> (b -> a) -> BinTree' b -> a
foldrBT' f v (Leaf a) = v a
foldrBT' f v (Node' ln rn) = f ls rs
    where
        ls = foldrBT' f v ln
        rs = foldrBT' f v rn

-- 2.1
nodes' = foldrBT' (\x y -> 1+x+y) (\_ -> 0)

-- 2.2
height' = foldrBT' (\x y -> 1 + max x y) (\_ -> 1)

-- 2.3
umbalance' = foldrBT' (umbAux') (\_ -> (1, 0))
    where
        umbAux' (lh, lu) (rh, ru) = (height, umb)
            where
                height = 1 + max lh rh
                umb = max (max lu ru) (abs (lh - rh))

-- Albero ennario
data Tree a = R a [Tree a]
    deriving (Show)

mapT :: (a -> b) -> Tree a -> Tree b
mapT f (R a ts) = R (f a) (map (mapT f) ts)

foldrT :: (a -> [b] -> b) -> [b] -> Tree a -> b
foldrT f v (R a []) = f a v
foldrT f v (R a ts) = f a (map (foldrT f v) ts)


-- Esercizio 3 su albero binario
-- Utilizzo foldrBT dell'esercizio precedente.

diameter = snd . foldrBT (diameterAux) (0, 0)
    where
        diameterAux (lh, ld) _ (rh, rd) = (h, d)
            where
                h = 1 + max lh rh
                -- Verifico il massimo diametro tra quelle ritornate dagli alberi dx e sx
                -- o se il massimo diametro è quella che passa dal nodo calcolandola
                -- come altezza sx + altezza dx + 1 (il nodo in cui si sta)
                d = max (max ld rd) (1 + lh + rh)

t1 = Node 1 (Node 2 (Node 3 (Node 4 Empty Empty) (Node 5 Empty Empty)) (Node 6 (Node 7 Empty Empty) (Node 8 Empty Empty))) (Node 9 (Node 10 (Node 11 Empty Empty) (Node 12 Empty Empty)) (Node 13 (Node 14 Empty Empty) (Node 15 Empty Empty)))

-- t2 è l'albero in cui il cammino più lungo non passa per la radice perchè  è sbilanciato a sinistra e il sotto albero dx ha solamente 1 nodo.

t2 = Node 1 (Node 2 (Node 3 (Node 4 Empty Empty) (Node 5 (Node 42 (Node 43 (Node 44 Empty Empty) Empty) Empty) Empty)) (Node 6 (Node 7 Empty Empty) (Node 8 (Node 66 (Node 67 (Node 68 Empty Empty) Empty) Empty) Empty))) (Node 9 Empty Empty)


{- Esercizio 4

tails [] = [[]]
tails xs@(x:txs) = xs:tails txs

map f (x:xs) = f x:map f xs
map _ [] = []

foldr f v = v
foldr f v (x:xs) = f x (foldr f v xs)

scanr f e = map (foldr f e) . tails

- Caso []
scanr f e [] =
map (foldr f e) . tails [] =
map (foldr f e) (tails []) =
    {def. di tails}
map (foldr f e) [[]]=
    {def. di map}
foldr f e []:map (foldr f e) [] =
    {def. di foldr}
e:map (foldr f e) [] =
    {def. di map}
e:[] =
[e]

- Caso x:xs
scanr f e (x:xs) =
map (foldr f e) (tails (x:xs))=
map (foldr f e) ((x:xs):tails xs))=
    {def. di map}
foldr f e (x:xs):map (fodr f e) (tails xs)=
    {per induzione}
foldr f e (x:xs):scanr f e xs
    {def foldr}
f x (foldr f e xs):scanr f e xs
    {dimostrato sotto}
f x (head (scanr f e xs)):scanr f e xs

Che possiamo riscrivere come:
scanr f e (x:xs) = pre:post
    where
        pre = f x (head post)
        post = scanr f e xs


head (scanr f e (x:xs)) =
    {def. scanr}
head (map (foldr f e) (tails (x:xs))) =
head (map (foldr f e) ((x:xs):tails xs)) =
head (foldr f e (x:xs):map (foldr f e) (tails xs)) =
    {def. head}
foldr f e (x:xs)

-}
