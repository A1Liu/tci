#include <stdio.h>

void print_a(int a) {
  if (a) {
    printf("%d\n", a);
  } else {
    printf("'a' is zero\n");
  }
}

int main(int argc, char **argv) {
  for (int a = 0; a < 10; a = a + 1) {
    int b = a;
    print_a(b);
  }

  {
    int c = 10;
    print_a(c);
  }
}
