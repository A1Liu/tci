#include <stdio.h>
#include <stdlib.h>

int main() {
  int *a = malloc(sizeof(int) * 100);
  int *b = malloc(sizeof(int) * 500);
  int *c = malloc(sizeof(int) * 100);
  free(b);
  *a = 10;
  c[99] = 1;
}
