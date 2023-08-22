#include <stdio.h>
#include <stdlib.h>
#include "ex3.h"

void printArrayPairs(Pair *v, int n) {
    for (int i=0; i<n; i++) {
        printf("v[%d] => succ: %d, prec: %d\n", i, v[i].succ, v[i].prec);
    }
}

void printPrimes(Pair *v, int n) {
    int pos;

    pos = 2;
    while (pos < n) {
        if (pos + v[pos].succ < n){
            printf("%d,", pos);
            pos += v[pos].succ;
        } else
            break;
    }
    printf("%d\n", pos);
}

/* Alloca il vettore di Pair e setta prec e succ a 1 */
void initialize(Pair **v, int n) {
    Pair *tmp;

    *v = malloc(sizeof(Pair) * (unsigned int) n);
    if (!(*v)){
        printf("Error while memory allocation\n");
        exit(1);
    }
    tmp = *v;
    while (n-- > 0) {
        tmp->succ = 1;
        tmp->prec = 1;
        tmp++;
    }
}

/*
 * Elimino tutti i multipli del numero n partendo dall'ultimo numero
 * dell'array ancora valido (superstite) e vado indietro seguento il
 * puntatore prec.
 */
void delete(Pair *v, int n, int *end, int size) {
    /* REQ: v != NULL, n >= 2, end < size, size >= n */
    int pos, offset;

    pos = *end; // Comincio dall'ultimo elemento
    while (pos >= n) {
        int toDel = pos * n; // Posizione dell'elemento da eliminare
        if (toDel >= size) { // Sto uscendo dall'array
            pos = pos - v[pos].prec;
        } else {
            offset = toDel - v[toDel].prec;
            v[offset].succ += v[toDel].succ; // Aggiorno il collegamento dell'elemento precedente

            if (toDel != *end) {
                offset = toDel + v[toDel].succ;
                v[offset].prec += v[toDel].prec; // Aggiorno il collegamento dell'elemento successivo
            } else { // Sto eliminando l'ultimo elemento valido dell'array
                *end = offset; // Aggiorno ultimo elemento
            }
            v[toDel].succ = -1; // Invalido l'elemento attuale
            v[toDel].prec = -1;

            pos = pos - v[pos].prec;
        }
    }
}

Pair *eulersieve(int size) {
    /* REQ: size > 2 */
    int prime;
    int end;
    Pair *v;

    initialize(&v, size);
    prime = 2;
    end = size - 1;

    /* INV: v[prime] è un numero primo */
    while (prime < size) {
        delete(v, prime, &end, size);
        prime += v[prime].succ;
    }
    return v;
}

int main(int argc, char **argv) {
    if (argc != 2) {
        printf("USAGE: %s <UPPER_LIMIT (excluded)>\n", argv[1]);
        exit(1);
    }
    int n = atoi(argv[1]);
    if (n < 2) {
        printf("The first prime number is 2!\n");
        exit(1);
    }
    Pair *v = eulersieve(n);
    printPrimes(v, n);
}
