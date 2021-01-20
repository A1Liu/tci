#ifndef __TCI_STDIO_H
#define __TCI_STDIO_H

#include <stdarg.h>

typedef unsigned long size_t;
typedef unsigned long fpos_t;

typedef struct {
  // the current stream position and multibyte conversion state (an object of
  // type mbstate_t)
  fpos_t position;

  // the buffer
  char *buffer;
  unsigned int buffer_position;
  unsigned int buffer_capacity;

  // reentrant lock (required as of C11)
  unsigned int lock;

  // platform-specific identifier of the associated I/O device, such as a file
  // descriptor
  int fd;

  // error indicator
  int error;

  // bit numberings from low to high
  //
  // bits 0,1 character width (unset, narrow, or wide)
  //
  // bits 2,3 stream buffering state indicator (unbuffered, line buffered, fully
  // buffered)
  //
  // bits 4,5 I/O mode indicator (input stream, output stream, or update stream)
  // bit 6 binary/text mode indicator
  // 7 end-of-file indicator
  unsigned char flags;
} FILE;

int printf(const char *format, ...);

int sprintf(char *buffer, const char *format, ...);
int snprintf(char *buffer, size_t count, const char *format, ...);

int vprintf(const char *format, va_list va);
int vsnprintf(char *buffer, size_t count, const char *format, va_list va);

int sscanf(const char *restrict buffer, const char *restrict format, ...);

int vsscanf(const char *restrict buffer, const char *restrict format,
            va_list vlist);

#endif
