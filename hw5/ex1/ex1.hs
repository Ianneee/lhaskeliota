{- Il confronto con la versione Haskell: l'idea che ho implementato
 - è la stessa del codice c, zippando la lista con degli interi
 - per ricordarmi l'ordine degli elementi.
 - Ordino quindi prima sul valore degli elementi e poi sulle posizioni.
 - Uso la stessa funzione merge modificata per fare l'ordine swappano
 - gli elementi della tupla tra loro.
 - La differenza principale tra la versione c e quella haskell probabilmente
 - è che ci ho messo molto meno tempo (oltre a molto meno codice) per
 - implementare la stessa idea.
 -}
mergeSort []  = []
mergeSort [x] = [x]
mergeSort xs  = merge (mergeSort ls) (mergeSort rs)
    where
        n = length xs `div` 2
        (ls, rs) = splitAt n xs

        merge [] ys = ys
        merge xs [] = xs
        merge xs@((x,xp):txs) ys@((y,yp):tys)
            | x == y       = merge xs tys
            | x < y        = (x, xp):merge txs ys
            | otherwise    = (y, yp):merge xs tys

swapTuple = \(x, y) -> (y, x)
eliminaDuplicati xs = map snd (mergeSort (map swapTuple (mergeSort (zip xs [0..]))))



