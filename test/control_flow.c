#include <stdio.h>

void print_a(int a) {
  if (a) {
    printf("%d\n", a);
  } else {
    printf("'a' is zero\n");
  }
}

int main(int argc, char **argv) {
  int a;
  for (a = 0; a < 10; a = a + 1) {
    print_a(a);
  }
}
