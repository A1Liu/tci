#include <stdio.h>

int main() {
  if (5 == 5 && 5 > 3) {
    printf("true\n");
  }
  if (5 == 5 || 5 > 3) {
    printf("true\n");
  }
  if (5 == 5 || 5 < 3) {
    printf("true\n");
  }
  if (5 == 6 || 5 > 3) {
    printf("true\n");
  }
}