int a;

struct a;

struct a {
  int hello;
  char goodbye;
  struct d {
    char b;
  } c;
  struct d b;
};

struct a b;

struct d c;

int main(int i, char **argv) {
  { return 12; }
  return 1;
}
