#include <stdarg.h>
#include <stddef.h>
#include <tci.h>

#include <stdio.h>

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
