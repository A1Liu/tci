#define NULL ((void *)0)

typedef unsigned long size_t;

void *malloc(size_t size);
void *realloc(void *buffer, size_t new_size);
void free(void *value);

void exit(int status);

double atof(const char *str);

int atoi(const char *str);
long atol(const char *str);
long long atoll(const char *str);

long strtol(const char *restrict str, char **restrict str_end, int base);
long long strtoll(const char *restrict str, char **restrict str_end, int base);

unsigned long strtoul(const char *restrict str, char **restrict str_end,
                      int base);
unsigned long long strtoull(const char *restrict str, char **restrict str_end,
                            int base);

float strtof(const char *restrict str, char **restrict str_end);
double strtod(const char *restrict str, char **restrict str_end);
// long double strtold(const char *restrict str, char **restrict str_end);
