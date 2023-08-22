/*
 * Main per l'esercizio 1.
 * Utilizzo la funzione removeRec vista a lezione scorrendo la
 * lista iniziale.
 */

#include <stdlib.h>
#include <stdio.h>
#include "ex1.h"

int main() {
    int vals[7] = {7, 1, 1, 3, 7, 1, 3};

    lista L = NULL;
    L = cons(vals[6], L);
    for (int i = 5; i >= 0; i--){
        L = cons(vals[i], L);
    }

    stampaElementiLista(L);
    removeDupRec(L);
    printf("\nDuplicates removed\n");
    stampaElementiLista(L);

    return 0;
}
