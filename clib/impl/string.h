#include <stdint.h>
#include <string.h>

void *memcpy(void *_dest, void *_src, size_t n) {
  char *src = (char *)_src;
  char *dest = (char *)_dest;

  for (int i = 0; i < n; i++)
    dest[i] = src[i];

  return dest;
}

size_t strlen(char *str) {
  size_t len = 0;
  for (; *str; len++, str = str + 1)
    ;

  return len;
}
