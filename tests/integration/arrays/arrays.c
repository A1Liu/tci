#include "arrays.h"
#include <stdlib.h>
#include <string.h>

String string_new(char *value) {
  int len = strlen(value);
  String str;
  str.size = len;
  if (len + 1 < INLINE_LEN) {
    str.data = NULL;
    for (int i = 0; i < len; i++)
      str._inline[i] = value[i];
    str._inline[len] = '\0';
  } else {
    str.data = malloc(sizeof(char) * (len + 1));
    for (int i = 0; i < len; i++)
      str.data->data[i] = value[i];
    str.data->data[len] = '\0';
  }

  return str;
}

char *string_data(String *str) {
  if (str->size < INLINE_LEN) {
    return str->_inline;
  } else {
    return &str->data->data[0];
  }
}
