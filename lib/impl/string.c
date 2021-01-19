#include <stdint.h>
#include <string.h>

void bcopy(const void *src, void *dest, size_t len) {
  if (dest < src) {
    const char *firsts = (const char *)src;
    char *firstd = (char *)dest;
    while (len--)
      *firstd++ = *firsts++;
  } else {
    const char *lasts = (const char *)src + (len - 1);
    char *lastd = (char *)dest + (len - 1);
    while (len--)
      *lastd-- = *lasts--;
  }
}

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
