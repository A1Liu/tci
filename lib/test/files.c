#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
  char *hello_world = "Hello, world!\n";
  FILE *fptr = fopen("/output.txt", "w");
  if (fptr == NULL) {
    perror("idk man");
    return 1;
  }

  fprintf(fptr, "%s", hello_world);
  fputs(hello_world, fptr);
  fclose(fptr);

  char buf[100];
  fptr = fopen("/output.txt", "r");
  fgets(buf, 100, fptr);
  fclose(fptr);

  if (strcmp(buf, hello_world)) {
    printf("left:  %s", buf);
    printf("right: %s", hello_world);
    return 1;
  }

  return 0;
}
