#include <stdio.h>

int *value() {
  int a = 12;
  return &a;
}

int main() { printf("%d\n", *value()); }
