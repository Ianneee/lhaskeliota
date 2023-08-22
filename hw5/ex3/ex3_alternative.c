/*
 * Come prima versione avevo pensato a questa in cui non sto
 * usando prec ma solo succ.
 * Elimino gli elementi partendo da sinistra ed andando ricorsivamente
 * con la funzione deleteMultiples a tutti i multipli del sopravvissuto
 * che posso raggiungere moltiplicandolo con il numero di cui sto eliminando
 * i multipli. Al ritorno della chiamata ricorsiva cancello il numero su cui
 * sono e sistemo i puntatori del successivvo e del precedente.
 * In questo modo non ho bisogno di usare il puntatore prec.
 */

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
 * Eliminando gli elementi da sinistra a destra posso avere il problema che
 * elimino un numero che mi può servire per raggiungerne altri. Per evitare questa
 * situazione vado avanti in maniera ricorsiva verso tutti questi numeri ed
 * al rientro li elimino aggiornando le Pair tra loro collegate.
 */
void deleteMultiples(Pair *v, int n, int pos, int size) {
    int offset;

    if (pos >= size) return; // Sono fuori dall'array
    if (v[pos].succ == -1) return; // Sono su un elemento eliminato

    deleteMultiples(v, n, n*pos, size);

    offset = pos - v[pos].prec;
    v[offset].succ += v[pos].succ; // Aggiorno il collegamento dell'elemento precedente

    offset = pos + v[pos].succ;
    v[offset].prec += v[pos].prec; // Aggiorno il collegamento dell'elemento successivo

    v[pos].succ = -1; // Invalido l'elemento attuale
    v[pos].prec = -1;
}

Pair *eulersieve(int size) {
    int prime;
    int survivor;
    Pair *v;

    initialize(&v, size);
    prime = 2;
    survivor = prime;

    while (prime < size) {
        if (prime*survivor < size) {
            deleteMultiples(v, prime, prime*survivor, size);
            survivor += v[survivor].succ;
        } else { // Sto andando fuori dall'array
            prime += v[prime].succ;
            survivor = prime;
        }
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
