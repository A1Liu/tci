#include "arrays.h"
#include <stdlib.h>
#include <string.h>

String string_new(char *value) {
  int len = strlen(value);
  String str = malloc(sizeof(int) + sizeof(char) * len + 1);

  for (int i = 0; i < len; i++) {
    str->data[i] = value[i];
  }
  str->data[len] = '\0';
  str->size = len;

  return str;
}
