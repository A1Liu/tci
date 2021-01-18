#include <stdarg.h>
#include <stddef.h>
#include <tci.h>

#include <stdio.h>

#pragma tci enable_builtins

size_t tci_var_size(void *_var) {
  size_t size = 0;
  for (char *var = _var; (__tci_builtin_push_dyn(&var, sizeof(var)),
                          __tci_builtin_ecall(TCI_ECALL_IS_SAFE));
       var = var + 1, size++)
    ;

  return size;
}

void tci_throw_error(const char *name, const char *message,
                     unsigned int skip_frames) {
  tci_ecall(TCI_ECALL_THROW_ERROR, name, message, skip_frames + 2);
}

void *tci_ecall(int ecall_num, ...) {
  va_list list;
  va_start(list, ecall_num);

  unsigned long diff = ((unsigned long)(unsigned)-1) + 1;
  void *next = ((char *)list.current) - diff;
  size_t size = tci_var_size(next);
  for (; size != 0;) {
    __tci_builtin_push_dyn(next, size);
    list.current = next;
    next = ((char *)list.current) - diff;
    size = tci_var_size(next);
  }

  va_end(list);

  return __tci_builtin_ecall(ecall_num);
}
