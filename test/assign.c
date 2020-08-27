
int printf(char *format, ...);

int main(int argc, char **argv) {
  int hello, goodbye = 0;
  printf("%d\n", goodbye);
  goodbye = 12;
  printf("%d\n", goodbye);
  hello = 13;
  printf("%d\n", hello);
}
