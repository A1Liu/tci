int a;

struct a;

struct d {
  char b;
};

struct a {
  int hello;
  char goodbye;
  struct d c;
  struct d b;
};

struct a b;

struct d c;

int hello_world();

int hello_world() { return 0; }

int main(int i, char **argv) { return hello_world(); }
