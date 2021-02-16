#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
  char buf[100], *hello_world = "Hello, world!\n";
  FILE *fptr = fopen("/output.txt", "w");
  size_t len = 0, expected_len = strlen(hello_world);

  if (fptr == NULL) {
    perror("idk man");
    return 1;
  }

  fprintf(fptr, "%s", hello_world);
  fclose(fptr);

  fptr = fopen("/output.txt", "r");
  fgets(buf, 100, fptr);
  fclose(fptr);

  if (strcmp(buf, hello_world)) {
    printf("left:  %s", buf);
    printf("right: %s", hello_world);
    return 1;
  }

  fptr = fopen("/output.txt", "r");
  int vars_written = fscanf(fptr, "%100c\n%zn", buf, &len);
  fclose(fptr);

  if (vars_written != 1) {
    printf("wrote: %d\n", vars_written);
    return 1;
  }

  if (len != expected_len) {
    printf("len was %zd, should've been %zd\n", len, expected_len);
    return 1;
  }

  buf[len] = '\0';

  if (strcmp(buf, hello_world)) {
    printf("left:  %s", buf);
    printf("right: %s", hello_world);
    return 1;
  }

  printf("Success!\n");
  return 0;
}
