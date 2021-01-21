#include <errno.h>
#include <stdio.h>
#include <stdlib.h>

int __tci_errno;

static char *errno_strs[] = {"no error right now! (errno is 0)",
                             "invalid parameter (EDOM)",
                             "return value doesn't fit in return type (ERANGE)",
                             "input wasn't valid UTF-8 (EILSEQ)"};

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
