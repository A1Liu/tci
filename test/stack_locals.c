#include <stdio.h>

int *value() {
  int a = 12;
  return &a;
}

int get_value() { return *value(); }

int main() { printf("%d\n", get_value()); }
