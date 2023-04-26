#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <tci.h>

static char *tci_errno_strs[] = {
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

static int tci_ecall_param_counts[] = {
    1, /* TCI_ECALL_EXIT */
    0, /* TCI_ECALL_ARGC */
    0, /* TCI_ECALL_ARGV */

    2, /* TCI_ECALL_OPEN_FD */
    4, /* TCI_ECALL_READ_FD */
    4, /* TCI_ECALL_WRITE_FD */
    3, /* TCI_ECALL_APPEND_FD */
    1  /* TCI_ECALL_FD_LEN */
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
  if (ecall_num >= sizeof(tci_ecall_param_counts) / sizeof(int)) {
    tci_throw_error(
        "InvalidEcall",
        "Called an invalid environment call (this is an error in TCI)", 1);
  }

  va_list list;
  va_start(list, ecall_num);

  int param_count = tci_ecall_param_counts[ecall_num];
  void *next;
  unsigned int size;

  for (int i = 0; i < param_count; i++) {
    next = __builtin_va_arg(&list);
    size = tci_var_size(next);

    __tci_builtin_push(size);
    __tci_builtin_push(next);
    __tci_builtin_op("PushDyn", sizeof(void));
  }

  va_end(list);

  __tci_builtin_push(ecall_num);
  return __tci_builtin_op("Ecall", sizeof(void *));
}

void tci_perror(char *prefix, int error) {
  if (error < sizeof(tci_errno_strs) / sizeof(char *)) {
    fprintf(stderr, "%s: %s\n", prefix, tci_errno_strs[error]);
  } else {
    fprintf(stderr, "%s: invalid error code (errno = %d)\n", prefix, error);
  }
}

char *tci_strerror(int error) {
  if (error < sizeof(tci_errno_strs) / sizeof(char *)) {
    return tci_errno_strs[error];
  }

  return NULL;
}

void *__builtin_va_arg(va_list *list) {
  __tci_builtin_push(list->__tci_va_current);

  list->__tci_va_current -= 1;

  void *ptr = __tci_builtin_op("TranslateStackId", sizeof(void *));

  return ptr;
}

void __builtin_va_end(va_list *list) { list->__tci_va_current = -1; }
