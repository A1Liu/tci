#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
  char *hello_world = "Hello, world!";
  FILE *fptr = fopen("/output.txt", "w");
  if (fptr == NULL) {
    perror("idk man");
    return 1;
  }

  fputs(hello_world, fptr);
  fclose(fptr);

  char buf[100];
  fptr = fopen("/output.txt", "r");
  fgets(buf, 100, fptr);
  fclose(fptr);

  if (strcmp(buf, hello_world)) {
    printf("left:  %s\n", buf);
    printf("right: %s\n", hello_world);
    return 1;
  }

  return 0;
}
