void goodbye() { return; }

void hello() { return goodbye(); }

int return_code() { return 10; }

int main(int i, char **argv) {
  hello();
  return return_code() - return_code();
}
