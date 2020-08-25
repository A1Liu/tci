
void goodbye() { return; }

void hello() { return goodbye(); }

int return_code(int hello) { return 10 + 12; }

int main(int i, char **argv) {
  hello();

  return return_code() - return_code();
}
