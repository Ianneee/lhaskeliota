#include <stdlib.h>
#include <stdio.h>
#include "ex1.h"

lista removeRec(lista L, int x){
    lista M;
    if (!L) {
        return NULL;
    }
    if (L->val == x){
        M = L->next;
        free(L);
        return removeRec(M, x);
    }
    L->next = removeRec(L->next, x);
    return L;
}

/* Rimuove i duplicati dalla lista, iterativo */
void removeDuplicates(lista L) {
    while (L) {
        L->next = removeRec(L->next, L->val);
        L = L->next;
    }
}

/* Rimuove i duplicati dalla lista, ricorsivo */
void removeDupRec(lista L) {
    if (!L) return;
    removeDupRec(L->next = removeRec(L->next, L->val));
}

