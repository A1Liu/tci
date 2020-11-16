#include "dyn_array_ptr.h"
#include <stdio.h>

typedef struct {
  uint64_t hello;
} Hello;

int main() {
  char *hello = dyn_array_new(char);
  dyn_array_declare(hello2, char);

  dyn_array_add(&hello, 'a');
  dyn_array_add(&hello, 'b');
  dyn_array_add(&hello, 'c');
  dyn_array_add(&hello, 'd');
  dyn_array_add(&hello, 'e');
  dyn_array_add(&hello, 'f');
  dyn_array_add(&hello, 'g');
  dyn_array_add(&hello, 'h');
  dyn_array_add(&hello, 'i');
  dyn_array_add(&hello, 'j');
  dyn_array_add(&hello, 'k');
  dyn_array_add(&hello, 'l');
  dyn_array_add(&hello, 'm');
  dyn_array_add(&hello, 'n');
  dyn_array_add(&hello, 'o');
  printf("%llu\n", dyn_array_capacity(hello));
  printf("%llu\n", dyn_array_len(hello));

  uint64_t len = dyn_array_len(hello);
  for (uint64_t i = 0; i < len; i++) {
    printf("%c ", hello[i]);
  }
  printf("\n");

  printf("main: %.*s\n", (uint32_t)dyn_array_len(hello), hello);

  dyn_array_add_from(&hello2, hello, dyn_array_len(hello));
  dyn_array_add_from(&hello2, hello, dyn_array_len(hello));
  uint64_t len2 = dyn_array_len(hello2);
  for (uint64_t i = 0; i < len2; i++) {
    printf("%c ", hello2[i]);
  }
  printf("\n");
}
