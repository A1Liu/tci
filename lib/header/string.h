typedef unsigned long size_t;

void *memset(void *dest, int ch, size_t count);

void bcopy(const void *src, void *dest, size_t len);

void *memcpy(void *dest, void *src, size_t n);

size_t strlen(char *str);
