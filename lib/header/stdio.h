#ifndef __TCI_STDIO_H
#define __TCI_STDIO_H

#include <stdarg.h>
#include <stddef.h>

int printf(const char *format, ...);

int sprintf(char *buffer, const char *format, ...);
int snprintf(char *buffer, size_t count, const char *format, ...);

int vprintf(const char *format, va_list va);
int vsnprintf(char *buffer, size_t count, const char *format, va_list va);

int sscanf(const char *restrict buffer, const char *restrict format, ...);

int vsscanf(const char *restrict buffer, const char *restrict format,
            va_list vlist);

#endif
