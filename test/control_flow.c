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
    if (b == 5) {
      continue;
    }

    print_a(b);

    {
      if (b == 8)
        break;
      else
        continue;
    }
  }

  for (int b = 0; b < 10; b = b + 1) {
    printf("%d", b);
  }

  for (int b = 0; b < 10; b = b + 1) {
    break;
    printf("%d", b);
  }

  printf("\n");

  {
    int c = 10;
    print_a(c);
  }
}
