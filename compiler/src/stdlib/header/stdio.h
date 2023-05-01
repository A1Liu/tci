#ifndef __TCI_STDIO_H
#define __TCI_STDIO_H

#include <stdarg.h>

typedef unsigned long size_t;
typedef unsigned int fpos_t;
// typedef struct {
//   char state[4];
//   unsigned int idx;
// } fpos_t;

// TODO remove definition from header
typedef struct __tci_file {
  // reentrant lock (required as of C11)
  unsigned long lock;

  // the buffer
  char *buffer;
  unsigned int buffer_pos;
  unsigned int buffer_readable_pos;
  unsigned int buffer_capacity;

  // the current stream position and multibyte conversion state (an object of
  // type mbstate_t)
  fpos_t position;

  // platform-specific identifier of the associated I/O device, such as a file
  // descriptor
  unsigned int fd;

  // error indicator
  int error;

  // Flags
  unsigned short flags;
} FILE;

extern FILE *__tci_stdout;
extern FILE *__tci_stderr;
extern FILE *__tci_stdin;

#define stdout __tci_stdout
#define stderr __tci_stderr
#define stdin __tci_stdin
#define BUFSIZ 1024
#define EOF (-1)

FILE *fopen(const char *filename, const char *mode);
int fclose(FILE *fp);
int remove(const char *filename);

int fputc(int c, FILE *fp);
int fputs(const char *s, FILE *fp);
int fflush(FILE *fp);

int fgetc(FILE *fp);
int ungetc(int c, FILE *fp);
char *fgets(char *restrict str, int count, FILE *restrict stream);
int feof(FILE *fp);

size_t fread(void *ptr, size_t size_of_elements, size_t number_of_elements,
             FILE *a_file);
size_t fwrite(const void *ptr, size_t size_of_elements,
              size_t number_of_elements, FILE *a_file);

void perror(const char *s);

int printf(const char *format, ...);
int fprintf(FILE *stream, const char *format, ...);
int vfprintf(FILE *stream, const char *format, va_list arg);

int sprintf(char *buffer, const char *format, ...);
int snprintf(char *buffer, size_t count, const char *format, ...);

int vprintf(const char *format, va_list va);
int vsnprintf(char *buffer, size_t count, const char *format, va_list va);

int sscanf(const char *restrict buffer, const char *restrict format, ...);

int vsscanf(const char *restrict buffer, const char *restrict format,
            va_list vlist);

int scanf(const char *fmt, ...);
int fscanf(FILE *fp, const char *fmt, ...);
int vfscanf(FILE *fp, const char *fmt, va_list ap);

#endif
