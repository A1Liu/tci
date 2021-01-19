#ifndef __TCI_STDIO_H
#define __TCI_STDIO_H

int printf(char *format, ...);

int sscanf(const char *restrict buffer, const char *restrict format, ...);

int vsscanf(const char *restrict buffer, const char *restrict format,
            va_list vlist);

#endif
