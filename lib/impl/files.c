#include <errno.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <tci.h>

#define FLAG_NARROW ((uint16_t)0x1)
#define FLAG_WIDE ((uint16_t)0x2)
#define FLAG_LINE_BUF ((uint16_t)0x4)
#define FLAG_FULL_BUF ((uint16_t)0x8)
#define FLAGS_BUF ((uint16_t)0xC)

#define FLAG_IO_INPUT ((uint16_t)0x10)
#define FLAG_IO_OUTPUT ((uint16_t)0x20)
#define FLAG_IO_APPEND ((uint16_t)0x40)
#define FLAGS_IO ((uint16_t)0x70)

#define FLAG_BINARY ((uint16_t)0x80)
#define FLAG_EOF ((uint16_t)0x100)
#define FLAG_CLOSED ((uint16_t)0x200)

#define FLAG_CREATE ((uint16_t)0x400)
#define FLAG_CLEAR ((uint16_t)0x800)

#define FLAG_STDIN_INIT ((uint16_t)0x14)  // FLAG_IO_INPUT + FLAG_LINE_BUF
#define FLAG_STDOUT_INIT ((uint16_t)0x44) // FLAG_IO_APPEND + FLAG_LINE_BUF

#define tci_buffer(fp)                                                         \
  ((char *)((fp)->buffer_capacity <= 8U ? (void *)&(fp)->buffer : (fp)->buffer))

static char __tci_stdin_buffer[BUFSIZ];
static FILE __tci_stdin_struct = {0, __tci_stdin_buffer, 0, 0, BUFSIZ, 0, 0,
                                  0, FLAG_STDIN_INIT};

static char __tci_stdout_buffer[BUFSIZ];
static FILE __tci_stdout_struct = {0, __tci_stdout_buffer, 0, 0, BUFSIZ, 0, 1,
                                   0, FLAG_STDOUT_INIT};

static char __tci_stderr_buffer[BUFSIZ];
static FILE __tci_stderr_struct = {0, __tci_stderr_buffer, 0, 0, BUFSIZ, 0, 2,
                                   0, FLAG_STDOUT_INIT};

FILE *__tci_stdout = &__tci_stdout_struct;
FILE *__tci_stderr = &__tci_stderr_struct;
FILE *__tci_stdin = &__tci_stdin_struct;

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
  flags |= FLAG_FULL_BUF;
  uint32_t open_mode = should_create + should_clear;

  uint64_t fd = tci_ecall(TCI_ECALL_OPEN_FD, name, open_mode);
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

  default:
    tci_throw_error("InvalidFileError",
                    "got the wrong file error (this is a bug in TCI)", 0);
  }

  FILE *fp = (uint8_t *)malloc(8U + sizeof(FILE) + BUFSIZ) + 8U;
  fp->buffer = (uint8_t *)(fp + 1);
  fp->buffer_pos = 0;
  fp->buffer_readable_pos = 0;
  fp->buffer_capacity = BUFSIZ;
  fp->position = 0;

  fp->lock = 0;
  fp->fd = fd;
  fp->error = 0;
  fp->flags = flags;

  return fp;
}

int fclose(FILE *fp) {
  const uint16_t output_mask = FLAG_IO_OUTPUT | FLAG_IO_APPEND;
  if (fp->flags & output_mask) {
    int err = fflush(fp);
    if (err)
      return err;
  }

  if (fp->fd < 4) {
    fp->flags &= FLAG_CLOSED;
    return 0;
  }

  free((char *)fp - 8U);
  return 0;
}

// Assumes flags have already been checked for errors
static inline uint64_t tci_write_out(unsigned int fd, fpos_t position,
                                     char *buffer, unsigned int len,
                                     uint16_t flags) {
  if (flags & FLAG_IO_OUTPUT) {
    __tci_builtin_push(fd);
    __tci_builtin_push(position);
    __tci_builtin_push(buffer);
    __tci_builtin_push(len);

    __tci_builtin_push(TCI_ECALL_WRITE_FD);
  } else {
    __tci_builtin_push(fd);
    __tci_builtin_push(buffer);
    __tci_builtin_push(len);

    __tci_builtin_push(TCI_ECALL_APPEND_FD);
  }

  uint64_t result = __tci_builtin_op("Ecall", sizeof(uint64_t));
  switch (result >> 32) {
  case 0:
    break;

  case TCI_FILE_ERR_DOESNT_EXIST:
    tci_throw_error("FileWasDeleted",
                    "file was deleted while we still had it open", 2);

  case TCI_FILE_ERR_OUT_OF_RANGE:
    tci_throw_error("FileIndexInvalid",
                    "wrote outside of file's length (this is a bug in TCI)", 0);

  case TCI_FILE_ERR_FILES_TOO_LARGE:
    return (uint64_t)TCI_ERRNO_FILES_TOO_LARGE << 32;

  default:
    tci_throw_error("InvalidFileError",
                    "got the wrong file error (this is a bug in TCI)", 0);
  }

  return (flags & FLAG_IO_OUTPUT) ? position + len : result;
}

// Assumes flags have already been checked for errors
static inline uint64_t tci_read_in(unsigned int fd, fpos_t position,
                                   char *buffer, unsigned int len,
                                   uint16_t flags) {
  __tci_builtin_push(fd);
  __tci_builtin_push(position);
  __tci_builtin_push(buffer);
  __tci_builtin_push(len);

  __tci_builtin_push(TCI_ECALL_READ_FD);

  uint64_t result = __tci_builtin_op("Ecall", sizeof(uint64_t));
  switch (result >> 32) {
  case 0:
    break;

  case TCI_FILE_ERR_DOESNT_EXIST:
    tci_throw_error("FileWasDeleted",
                    "file was deleted while we still had it open", 2);

  case TCI_FILE_ERR_OUT_OF_RANGE:
    tci_throw_error(
        "FileIndexInvalid",
        "tried to read outside of file's length (this is a bug in TCI)", 0);

  default: {
    tci_perror("internal TCI error", result >> 32);

    tci_throw_error("InvalidFileError",
                    "got the wrong file error (this is a bug in TCI)", 0);
  }
  }

  return result;
}

int fputc(int _c, FILE *fp) {
  if (fp->flags & FLAG_CLOSED)
    tci_throw_error("FileIsClosed", "tried to use an already closed file", 1);

  const uint16_t output_mask = FLAG_IO_OUTPUT | FLAG_IO_APPEND;
  if (!(fp->flags & output_mask))
    tci_throw_error("FileIsNotOutput",
                    "tried to flush a file that isn't an output file", 1);

  char c = _c;
  if (!(fp->flags & FLAGS_BUF)) {
    uint64_t ret = tci_write_out(fp->fd, fp->position++, &c, 1, fp->flags);
    if (ret >> 32) {
      fp->error = ret >> 32;
      return EOF;
    }

    return c;
  }

  if (fp->buffer_pos == fp->buffer_capacity) {
    int ret = fflush(fp);
    if (ret)
      return ret;
  }

  tci_buffer(fp)[fp->buffer_pos++] = c;

  if (c == '\n' && (fp->flags & FLAG_LINE_BUF)) {
    int ret = fflush(fp);
    if (ret)
      return ret;
  }

  return c;
}

int fputs(const char *s, FILE *fp) {
  for (; *s; s++)
    if (fputc(*s, fp) == EOF)
      return EOF;

  return 1;
}

int fflush(FILE *fp) {
  // TODO handle null pointer case

  if (fp->flags & FLAG_CLOSED)
    tci_throw_error("FileIsClosed", "tried to use an already closed file", 1);

  const uint16_t output_mask = FLAG_IO_OUTPUT | FLAG_IO_APPEND;
  if (!(fp->flags & output_mask))
    tci_throw_error("FileIsNotOutput",
                    "tried to flush a file that isn't an output file", 1);

  if (fp->buffer_pos == 0) // nothing to flush
    return 0;

  uint64_t ret = tci_write_out(fp->fd, fp->position, tci_buffer(fp),
                               fp->buffer_pos, fp->flags);
  if (ret >> 32) {
    fp->error = ret >> 32;
    return EOF;
  }

  fp->position = ret;
  fp->buffer_pos = 0;
  fp->buffer_readable_pos = 0;
  return 0;
}

int fgetc(FILE *fp) {
  if (fp->flags & FLAG_CLOSED)
    tci_throw_error("FileIsClosed", "tried to use an already closed file", 1);

  if (!(fp->flags & FLAG_IO_INPUT))
    tci_throw_error("FileIsNotInput",
                    "tried to fgetc a file that isn't an input file", 1);

  if (fp->flags & FLAG_EOF)
    return EOF;

  char *buffer = tci_buffer(fp);

  if (fp->buffer_pos < fp->buffer_readable_pos)
    return buffer[fp->buffer_pos++];

  if (!(fp->flags & FLAGS_BUF)) {
    uint64_t ret = tci_read_in(fp->fd, fp->position++, buffer, 1, fp->flags);

    if (ret >> 32) {
      fp->error = ret >> 32;
      return EOF;
    }

    if (ret == 0) {
      fp->flags |= FLAG_EOF;
      return EOF;
    }

    return *buffer;
  }

  const uint64_t ret =
      tci_read_in(fp->fd, fp->position += fp->buffer_readable_pos, buffer,
                  fp->buffer_capacity, fp->flags);

  if (ret >> 32) {
    fp->error = ret >> 32;
    return EOF;
  }

  if (ret == 0) {
    fp->flags |= FLAG_EOF;
    return EOF;
  }

  fp->buffer_pos = 1;
  fp->buffer_readable_pos = ret;

  return *buffer;
}

int ungetc(int c, FILE *fp) {
  if (fp->flags & FLAG_CLOSED)
    tci_throw_error("FileIsClosed", "tried to use an already closed file", 1);

  if (!(fp->flags & FLAG_IO_INPUT))
    tci_throw_error("FileIsNotInput",
                    "tried to ungetc a file that isn't an input file", 1);

  if (c == EOF)
    return c;

  fp->flags &= ~FLAG_EOF;
  char *buffer = tci_buffer(fp);

  if (fp->buffer_pos)
    return buffer[--fp->buffer_pos] = c;

  return EOF;
}

char *fgets(char *restrict str, int count, FILE *restrict fp) {
  char *original = str;
  for (int i = 1, c; i < count; i++, str++) {
    switch (c = fgetc(fp)) {
    case '\n':
      *str++ = '\n';
    case EOF:
      goto end;
    default:
      *str = c;
    }
  }

end:
  *str = '\0';
  return original;
}

int feof(FILE *fp) { return fp->flags & FLAG_EOF; }

size_t fread(void *ptr, size_t size_of_elements, size_t number_of_elements,
             FILE *a_file);
size_t fwrite(const void *ptr, size_t size_of_elements,
              size_t number_of_elements, FILE *a_file);
