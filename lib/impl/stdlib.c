#include <stdlib.h>
#include <tci.h>

int __tci_errno;

void *malloc(size_t size) { return tci_ecall(TCI_ECALL_HEAP_ALLOC, size, 2); }

void *realloc(void *_buffer, size_t new_size) {
  char *buffer = _buffer;

  char *alloc_begin = tci_ecall(TCI_ECALL_ALLOC_BEGIN, buffer);

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

void free(void *buffer) { return tci_ecall(TCI_ECALL_HEAP_DEALLOC, buffer, 2); }

void exit(int status) { tci_ecall(TCI_ECALL_EXIT, status); }
