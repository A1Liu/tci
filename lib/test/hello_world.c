int printf(char *c, ...);

void goodbye() { return; }

void hello(int first, ...) { return goodbye(); }

int return_code(int hi);

int main() {
  hello(12);
  //  printf("%s, %s!\n", "Hello", "world");
  printf("Hello, world!\n");

  return return_code(12) - return_code(12);
}

int return_code(int hello) { return hello + 12; }
