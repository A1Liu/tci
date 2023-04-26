#include <stdlib.h>
#include <tci.h>

void *malloc(size_t size) {
  __tci_builtin_push(size);
  __tci_builtin_push(2);
  return __tci_builtin_op("HeapAlloc", sizeof(void *));
}

void *realloc(void *_buffer, size_t new_size) {
  char *buffer = _buffer;

  __tci_builtin_push(buffer);
  char *alloc_begin = __tci_builtin_op("AllocBegin", sizeof(char *));

  if (buffer != alloc_begin)
    tci_throw_error(
        "InvalidPointer",
        "called realloc on a pointer in the middle of an allocation", 1);

  size_t old_size = tci_var_size(buffer);
  char *new_buffer = malloc(new_size);
  size_t copy_bound = new_size < old_size ? new_size : old_size;

  for (size_t i = 0; i < copy_bound; i++)
    new_buffer[i] = buffer[i];

  free(buffer);

  return new_buffer;
}

void free(void *buffer) {
  __tci_builtin_push(buffer);
  __tci_builtin_push(2);
  return __tci_builtin_op("HeapDealloc", sizeof(void *));
}

void exit(int status) { tci_ecall(TCI_ECALL_EXIT, status); }
