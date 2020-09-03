#include <stdio.h>

void print_a(int a) {
  if (a) {
    printf("%d\n", a);
  } else {
    printf("'a' is zero\n");
  }
}

int main(int argc, char **argv) {
  int a = 12;
  print_a(a);
  a = 0;
  print_a(a);
}
