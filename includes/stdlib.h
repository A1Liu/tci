#define NULL ((void *)0)

typedef unsigned long size_t;

void *malloc(size_t size);
void *realloc(void *buffer, size_t new_size);
void free(void *value);
