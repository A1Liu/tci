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
  if (errno < sizeof(errno_strs) / sizeof(char *)) {
    printf("%s: %s\n", prefix, errno_strs[errno]);
  } else {
    printf("%s: invalid error code (errno = %d)\n", prefix, errno);
  }
}

char *strerror(int error) {
  if (errno < sizeof(errno_strs) / sizeof(char *)) {
    return errno_strs[errno];
  }

  return NULL;
}
