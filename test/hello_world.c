#include <stdio.h>

void goodbye() { return; }

void hello(...) { return goodbye(); }

int return_code(int hi);

int main() {
  hello();
  printf("%s, %s!\n", "Hello", "world");

  return return_code(12) - return_code(12);
}

int return_code(int hello) { return hello + 12; }
