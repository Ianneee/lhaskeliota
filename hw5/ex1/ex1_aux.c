/*
 * Funzioni ausiliarie
 */

#include <stdlib.h>
#include <stdio.h>
#include "ex1.h"

lista cons(int x, lista L){
    lista M = (lista) malloc(sizeof(listanode));
    M->val = x;
    M->next = L;
    return M;
}

int length(lista L) {
    if (!L) {
        return 0;
    }
    return 1 + length(L->next);
}

void stampaElementiLista(lista L) {
    int len = length(L);
    for (int i = 0; i < len; i++){
        printf("Nodo %d: %d\n", i, L->val);
        L = L->next;
    }
}

int lengthListaInfo(listaInfo L) {
    if (!L) {
        return 0;
    }
    return 1 + lengthListaInfo(L->next);
}

void stampaElementiListaInfo(listaInfo L) {
    int len = lengthListaInfo(L);
    for (int i = 0; i < len; i++){
        printf("Nodo %d - pos:%d, val referred: %d\n", i, L->pos, L->node->val);
        L = L->next;
    }
}

