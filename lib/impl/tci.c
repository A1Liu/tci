#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <tci.h>

static char *__tci_errno_strs[] = {
    /* 0 */ "no error right now!",
    /* 1 */ "file doesn't exist",
    /* 2 */ "file name isn't UTF-8",
    /* 3 */ "too many files (maximum is 4000)",
    /* 4 */ "files too large (maximum total size is 64MB)",
    /* 5 */ "file seeking was out of range",
    /* 6 */ "reading from a terminal output (standard output)",
    /* 7 */ "reading from a terminal output (standard error)",
    /* 8 */ "reading from a terminal output (standard log)",
    /* 9 */ "writing to a terminal input (standard input)",
};

size_t tci_var_size(void *var) {
  __tci_builtin_push(var);
  __tci_builtin_push(var);
  char *begin = __tci_builtin_op("AllocBegin", sizeof(char *));
  char *end = __tci_builtin_op("AllocEnd", sizeof(char *));
  if (begin == NULL || end == NULL) {
    return -1;
  }

  return end - begin;
}

void tci_throw_error(const char *name, const char *message,
                     unsigned int skip_frames) {
  __tci_builtin_push(name);
  __tci_builtin_push(message);
  __tci_builtin_push(skip_frames + 1);
  __tci_builtin_op("Throw", sizeof(void));
}

void *tci_ecall(int ecall_num, ...) {
  va_list list;
  va_start(list, ecall_num);

  const unsigned long diff = ((unsigned long)(unsigned)-1) + 1;
  void *next = ((char *)list.current) - diff;
  size_t size = tci_var_size(next);

  while (size != -1) {
    __tci_builtin_push((unsigned int)size);
    __tci_builtin_push(next);
    __tci_builtin_op("PushDyn", sizeof(void));

    list.current = next;
    next = ((char *)list.current) - diff;
    size = tci_var_size(next);
  }

  va_end(list);

  __tci_builtin_push(ecall_num);
  return __tci_builtin_op("Ecall", sizeof(void *));
}

void tci_perror(char *prefix, int error) {
  if (error < sizeof(__tci_errno_strs) / sizeof(char *)) {
    fprintf(stderr, "%s: %s\n", prefix, __tci_errno_strs[error]);
  } else {
    fprintf(stderr, "%s: invalid error code (errno = %d)\n", prefix, error);
  }
}

char *tci_strerror(int error) {
  if (error < sizeof(__tci_errno_strs) / sizeof(char *)) {
    return __tci_errno_strs[error];
  }

  return NULL;
}
