#ifndef EX1_HEADER
#define EX1_HEADER

typedef struct L {
    int val;
    struct L* next;
} listanode;

typedef listanode* lista;

typedef struct L2 {
    lista node;
    int pos;
    struct L2 *next;
} listaInfoNode;

typedef listaInfoNode* listaInfo;

// Esercizio O(n^2)
lista removeRec(lista L, int x);
void removeDuplicates(lista L);
void removeDupRec(lista L);

// Funzioni ausiliarie
lista cons(int x, lista L);
int length(lista L);
void stampaElementiLista(lista L);

int lengthListaInfo(listaInfo L);
void stampaElementiListaInfo(listaInfo L);
listaInfo mergeSort(listaInfo L, int positions);

// Esercizio O(nlogn)
listaInfo copyData(lista L, int pos);
void ricostruisciLista(listaInfo L);

#endif
