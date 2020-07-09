int a = 12;

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

int hello_world(void);

int hello_world(void) { return 0; }

int main(int i, char **argv) { return hello_world(); }
