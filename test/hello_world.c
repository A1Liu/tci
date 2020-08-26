
void printf(char *str, ...);

void goodbye() { return; }

void hello(...) { return goodbye(); }

int return_code(int hello) { return hello + 12; }

int main(int i, char **argv) {
  hello();
  printf("Hello, world!\n");

  return return_code(12) - return_code(12);
}
