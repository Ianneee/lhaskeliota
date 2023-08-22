#include <stdio.h>
#include <stdlib.h>
#include "ex2.h"

cBinTree cbinT(int n, int k) {
    cBinTree R = malloc(sizeof(cBinTreeNode));
    if (!R) {
        printf("Malloc error\n");
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
    R->left = cbinT(n-1, k-1);
    R->right = cbinT(n-1, k);

    R->val = R->left->val + R->right->val;
    return R;
}

int cbin(int n, int k) {
    /*
    if (n == k || k == 0) return 1;
    return cbin(n-1, k-1) + cbin(n-1, k);
    */
    return cbinT(n, k)->val;
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
    printf("Result: %d\n", cbin(a, b));
}
