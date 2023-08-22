#include <stdio.h>
#include <stdlib.h>
#include "ex1.h"

/*
 * Creo la struct col le informazioni riguardo la lista passata come argomento:
 * node: puntatore al nodo della lista
 * pos: posizione del nodo nella lista
 * next: puntatore alla prossima struct listaInfo
 */
listaInfo copyData(lista L, int pos) {
    if (!L) return NULL;

    listaInfo N = (listaInfo) malloc(sizeof(listaInfo));

    N->node = L;
    N->pos = pos;
    N->next = copyData(L->next, pos+1);
    return N;
}

/* Ordina sui valori dei nodi della lista originaria */
listaInfo merge(listaInfo L, listaInfo M) {
    if (!M) return L;
    if (!L) return M;
    if (L->node->val < M->node->val) {
        L->next = merge(L->next, M);
        return L;
    } else if (L->node->val > M->node->val) {
        M->next = merge(L, M->next);
        return M;
    } else { // I nodi della lista hanno valore uguale
        listaInfo next = M->next;
        free(M->node); // Cancella il nodo dalla lista
        free(M);
        return merge(L, next);
    }
}

/* Ordina sulle posizioni dei nodi della lista originaria */
listaInfo mergePositions(listaInfo L, listaInfo M) {
    if (!M) return L;
    if (!L) return M;
    if (L->pos <= M->pos) {
        L->next = mergePositions(L->next, M);
        return L;
    } else {
        M->next = mergePositions(L, M->next);
        return M;
    }
}

void dividInPlace(listaInfo L, listaInfo *D, listaInfo *P) {
    if (L && L->next) {
        dividInPlace(L->next->next, D, P);
        L->next->next = *P;
        *P = L->next;
        L->next = *D;
        *D =L;
    } else {
        *D = L;
        *P = NULL;
    }
}

/*
 * Ordina la listaInfo passata come orgamento, se positions è 0 Ordina
 * sui valori dei nodi puntati della lista originale, se positions è 1
 * ordina sulla posizioni originali dei nodi puntati.
 * */
listaInfo mergeSort(listaInfo L, int positions) {
    if (!L) {
        return NULL;
    } else if (L->next == NULL) { // Mancava questo caso
        return L;
    }
    listaInfo D, P, K;
    dividInPlace(L, &D, &P);

    D = mergeSort(D, positions);
    P = mergeSort(P, positions);
    if (!positions)
        return merge(D, P);
    else
        return mergePositions(D, P);
}

/*
 * Relink dei nodi superstiti della lista originale.
 */
void ricostruisciLista(listaInfo L) {
    while (L->next != NULL) {
        L->node->next = L->next->node;
        L = L->next;
    }
}
