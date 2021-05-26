#include <errno.h>
#include <stdio.h>
#include <stdlib.h>

int __tci_errno;

static char *errno_strs[] = {
    /* 0 */ "no error right now! (errno is 0)",
    /* 1 */ "invalid parameter (EDOM)",
    /* 2 */ "return value doesn't fit in return type (ERANGE)",
    /* 3 */ "input wasn't valid UTF-8 (EILSEQ)",
    /* 4 */ "file doesn't exist",
    /* 5 */ "too many files (maximum is 4000)",
    /* 6 */ "files too large (maximum total size is 64MB)",
};

void perror(char *prefix) {
  if (__tci_errno < sizeof(errno_strs) / sizeof(char *)) {
    fprintf(stderr, "%s: %s\n", prefix, errno_strs[__tci_errno]);
  } else {
    fprintf(stderr, "%s: invalid error code (errno = %d)\n", prefix,
            __tci_errno);
  }
}

char *strerror(int error) {
  if (error < sizeof(errno_strs) / sizeof(char *)) {
    return errno_strs[error];
  }

  return NULL;
}
