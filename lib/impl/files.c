#include <errno.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <tci.h>

#define FLAG_NARROW ((uint16_t)1)
#define FLAG_WIDE ((uint16_t)2)
#define FLAG_LINE_BUF ((uint16_t)4)
#define FLAG_FULL_BUF ((uint16_t)8)

#define FLAGS_IO ((uint16_t)112) // 16 + 32 + 64
#define FLAG_IO_INPUT ((uint16_t)16)
#define FLAG_IO_OUTPUT ((uint16_t)32)
#define FLAG_IO_APPEND ((uint16_t)64)

#define FLAG_BINARY ((uint16_t)128)
#define FLAG_EOF ((uint16_t)256)

#define FLAG_CREATE ((uint16_t)512)
#define FLAG_CLEAR ((uint16_t)1024)

#define FLAG_STDOUT_INIT ((uint16_t)68) // FLAG_IO_APPEND + FLAG_LINE_BUF

char __tci_stdout_buffer[BUFSIZ];
FILE __tci_stdout_struct = {__tci_stdout_buffer, 0, BUFSIZ, 0, 0, 1, 0,
                            FLAG_STDOUT_INIT};
char __tci_stderr_buffer[BUFSIZ];
FILE __tci_stderr_struct = {__tci_stderr_buffer, 0, BUFSIZ, 0, 0, 2, 0,
                            FLAG_STDOUT_INIT};

FILE *__tci_stdout = &__tci_stdout_struct;
FILE *__tci_stderr = &__tci_stderr_struct;
FILE *__tci_stdin;

static inline uint16_t fopen_mode(const char *mode) {
  if (!strcmp(mode, "r"))
    return FLAG_IO_INPUT;
  if (!strcmp(mode, "rb"))
    return FLAG_IO_INPUT | FLAG_BINARY;
  if (!strcmp(mode, "r+"))
    return FLAG_IO_INPUT | FLAG_IO_OUTPUT;
  if (!strcmp(mode, "rb+") && !strcmp(mode, "rb+"))
    return FLAG_IO_INPUT | FLAG_IO_OUTPUT | FLAG_BINARY;

  if (!strcmp(mode, "w"))
    return FLAG_IO_OUTPUT | FLAG_CREATE | FLAG_CLEAR;
  if (!strcmp(mode, "wb"))
    return FLAG_IO_OUTPUT | FLAG_BINARY | FLAG_CREATE | FLAG_CLEAR;
  if (!strcmp(mode, "w+"))
    return FLAG_IO_INPUT | FLAG_IO_OUTPUT | FLAG_CREATE | FLAG_CLEAR;
  if (!strcmp(mode, "wb+") || !strcmp(mode, "w+b"))
    return FLAG_IO_INPUT | FLAG_IO_OUTPUT | FLAG_BINARY | FLAG_CREATE |
           FLAG_CLEAR;

  if (!strcmp(mode, "a"))
    return FLAG_IO_APPEND | FLAG_CREATE;
  if (!strcmp(mode, "ab"))
    return FLAG_IO_APPEND | FLAG_BINARY | FLAG_CREATE;
  if (!strcmp(mode, "a+"))
    return FLAG_IO_INPUT | FLAG_IO_APPEND | FLAG_CREATE;
  if (!strcmp(mode, "ab+") || !strcmp(mode, "a+b"))
    return FLAG_IO_INPUT | FLAG_IO_APPEND | FLAG_BINARY | FLAG_CREATE;

  tci_throw_error("InvalidFileMode", "file mode was invalid", 2);
}

FILE *fopen(const char *name, const char *mode) {
  uint16_t flags = fopen_mode(mode);
  bool should_create = (flags & FLAG_CREATE) != 0,
       should_clear = (flags & FLAG_CLEAR) != 0;
  flags &= ~(FLAG_CLEAR | FLAG_CREATE); // clear the flags
  uint32_t open_mode = should_create + should_clear;

  unsigned long fd = tci_ecall(TCI_ECALL_OPEN_FD, name, open_mode);
  switch (fd >> 32) {
  case 0:
    break;

  case TCI_FILE_ERR_DOESNT_EXIST:
    errno = TCI_ERRNO_DOESNT_EXIST;
    return NULL;
  case TCI_FILE_ERR_NAME_NOT_UTF8:
    errno = EILSEQ;
    return NULL;
  case TCI_FILE_ERR_TOO_MANY_FILES:
    errno = TCI_ERRNO_TOO_MANY_FILES;
    return NULL;
  case TCI_FILE_ERR_FILES_TOO_LARGE:
    errno = TCI_ERRNO_FILES_TOO_LARGE;
    return NULL;

  case TCI_FILE_ERR_OUT_OF_RANGE:
  default:
    tci_throw_error("InvalidFileError", "something messed up", 0);
  }

  FILE *file = malloc(sizeof(FILE) + BUFSIZ);
  file->buffer = (char *)(file + 1);

  return file;
}

int fclose(FILE *fp);

int fputc(int c, FILE *fp);
int fputs(const char *s, FILE *fp);

int fflush(FILE *fp);

int fgetc(FILE *fp);
char *fgets(char *buf, int n, FILE *fp);

size_t fread(void *ptr, size_t size_of_elements, size_t number_of_elements,
             FILE *a_file);
size_t fwrite(const void *ptr, size_t size_of_elements,
              size_t number_of_elements, FILE *a_file);
