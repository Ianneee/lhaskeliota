#include <stdio.h>
#include <stdlib.h>
#include "ex2.h"

/*
 * Per creare l'albero riutilizzando nodi già esistenti, faccio
 * una ricerca nell'albero prima di crearne di nuovi.
 */
cBinTree cbinTOpt(int n, int k, cBinTree root) {
    cBinTree F;
    cBinTree R = malloc(sizeof(cBinTreeNode));
    if (!R) {
        printf("Error while memory allocation\n");
        exit(1);
    }
    R->n = n;
    R->k = k;
    if (n == k || k == 0) {
        R->left = NULL;
        R->right = NULL;
        R->val = 1;
        return R;
    }

    if (!root) // Se sto creando la radice il parametro root sarà NULL
        root = R;

    F = betterFind(n-1, k-1, root); // cerco se esiste già un nodo da inserire come sinitro
    if (!F) { // Se non ho trovato nessun nodo lo creo
        R->left = cbinTOpt(n-1, k-1, root);
    } else {
        R->left = F;
    }

    /* Se la radice che sto usando per fare la ricerca, ha il ramo sinistro che sto
     * ancora completando (per esempio sto costruendo il ramo sinistro più esterno
     * partendo dalla radice dell'albero), passandogli questo nodo come radice non
     * produrrà nessun elemento nella ricerca perchè il left sarà ancora NULL
     * non essendo ancora ritornato dalle chiamate ricorsive.
     * Passo allora questo nodo stesso dato che avrà la parte sinistra appena completata.
     */
    if (root->left == NULL)
        root = R;
    F = betterFind(n-1, k, root);
    if (!F) {
        R->right = cbinTOpt(n-1, k, root);
    } else {
        R->right = F;
    }

    R->val = R->left->val + R->right->val;
    return R;
}

int cBinInvocationSharing(int a, int b) {
    cBinTree T = cbinTOpt(a, b, NULL);
    return T->val;
}


int main(int argc, char **argv) {
    if (argc != 3) {
        printf("USAGE: %s [INTEGER] [INTEGER]\n", argv[0]);
        exit(1);
    }
    int a = atoi(argv[1]);
    int b = atoi(argv[2]);
    if (a < b) {
        printf("Result: 0\n");
        return(0);
    }
    printf("Result: %d\n", cBinInvocationSharing(a, b));
}
