#include <strings.h>

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
