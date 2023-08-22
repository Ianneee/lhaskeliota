/*
 * Funzioni ausiliarie
 */

#include <stdlib.h>
#include <stdio.h>
#include "ex2.h"

/* Faccio una ricerca visitando tutto l'albero*/
cBinTree find(int n, int k, cBinTree T) {
    if (!T) return NULL;
    if (T->n == n && T->k == k)
        return T;
    cBinTree L, R;
    L = find(n, k, T->left);
    if (L) return L;

    R = find(n, k, T->right);
    if (R) return R;

    return NULL;
}

/* Invece di visitare tutto l'albero, posso scendere a destra
 * se il valore k di quel nodo è uguale al k che sto cercando
 * [(n, k-1)], a sinistra se n è più piccolo del valore n del
 * nodo, se invece il valore k o n del nodo è più piccolo
 * allora il valore che sto cercando non è nell'albero.
 */
cBinTree betterFind(int n, int k, cBinTree T) {
    if (!T) return NULL;
    if (T->n == n && T->k == k)
        return T;

    if (T->k == k) {
        return betterFind(n, k, T->right);
    }
    else if (n < T->n) {
        return betterFind(n, k, T->left);
    }
    else if (T->k < k || T->n < n)
        return NULL;
    else return NULL;
}


