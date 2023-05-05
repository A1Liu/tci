// Online C compiler to run C program online
#include "arrays.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {

  String a = string_new("hello!!");
  printf("String has inline size of: %ld\n", sizeof(a));
  printf("String of length %d: %s\n", a.size, string_data(&a));

  return 0;
}
