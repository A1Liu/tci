#include "statics.h"
#include <stdio.h>

static int private_function() {
  printf("Hello from private function in statics.c!\n");
}

void public_function() { private_function(); }
