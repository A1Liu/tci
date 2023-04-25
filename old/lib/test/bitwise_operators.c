#include <stdio.h>

int main() {
  int a = 5;
  int b = 1;
  int c = -5;
  printf("%d\n", a>>b);
  printf("%d\n", c>>b);
  printf("%d\n", a<<b);
  printf("%d\n", a&b);
  printf("%d\n", a|b);
  printf("%d\n", a^b);
}