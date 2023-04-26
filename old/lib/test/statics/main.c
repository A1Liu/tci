#include "statics.h"
#include <stdio.h>

static int private_function() {
  printf("Hello from private function in main.c!\n");
}

int main() {
  public_function();
  private_function();
  return 0;
}
