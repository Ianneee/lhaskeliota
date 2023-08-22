/*
 * Main per l'esercizio facoltativo.
 * Uso una struttura dati apposita L2 che incapsula la lista originale:
 * ha un puntatore ad un nodo della lista e la sua posizione nella lista.
 * Riordino L2 prima sui valori dei corrispettivi nodi puntati, poi
 * sulle posizioni originali ed infine scorrendo L2 faccio puntare i nodi
 * della lista originale ai nodi rimasti ricostruendo quindi la lista
 * senza duplicati mantenendo l'ordine iniziale.
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
    printf("Lista iniziale:\n");
    stampaElementiLista(L);

    listaInfo R = NULL;
    R = copyData(L, 0);
    R = mergeSort(R, 0);
    R = mergeSort(R, 1);

    ricostruisciLista(R);
    printf("\nLista ricostruita\n");
    stampaElementiLista(L);


    return 0;
}
