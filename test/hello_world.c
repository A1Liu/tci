
void goodbye() { return; }

void hello(...) { return goodbye(); }

int return_code(int hello) { return hello + 12; }

int main(int i, char **argv) {
  hello();

  return return_code(12) - return_code(12);
}
