#ifndef EX2_HEADER
#define EX2_HEADER

typedef struct B {
    struct B *left;
    int n;
    int k;
    int val;
    struct B *right;
} cBinTreeNode;

typedef cBinTreeNode* cBinTree;


cBinTree find(int n, int k, cBinTree T);
cBinTree betterFind(int n, int k, cBinTree T);

#endif
