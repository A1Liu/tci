void printf(char *format, ...);

int main(int argc, char **argv) {
  printf("%d\n", argc);
  argc = 12;
  printf("%d\n", argc);
}
