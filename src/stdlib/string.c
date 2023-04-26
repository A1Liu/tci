#include <string.h>
#include <tci.h>

void *memcpy(void *_dest, void *_src, size_t n) {
  char *src = (char *)_src;
  char *dest = (char *)_dest;

  for (int i = 0; i < n; i++)
    dest[i] = src[i];

  return dest;
}

void *memset(void *s, int _c, size_t n) {
  for (unsigned char *p = s, c = (unsigned char)_c; n--;)
    *p++ = c;

  return s;
}

size_t strnlen(const char *str, size_t max_len) {
  for (char *begin = str, *len = max_len; len--; str++)
    if (!*str)
      return str - begin;

  return max_len;
}

size_t strlen(char *str) {
  tci_assert_str(str);

  for (char *begin = str;; str++)
    if (!*str)
      return str - begin;
}

int strcmp(const char *left, const char *right) {
  tci_assert_str(left);
  tci_assert_str(right);

  for (; *left && !(*left - *right); left++, right++)
    ;

  return *left - *right;
}
