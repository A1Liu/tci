typedef struct {
  int size;
  char data[];
} * String;

String string_new(char *);
