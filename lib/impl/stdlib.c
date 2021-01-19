#include <errno.h>
#include <stdlib.h>
#include <tci.h>

void *malloc(size_t size) { return tci_ecall(TCI_ECALL_HEAP_ALLOC, size); }

void *realloc(void *_buffer, size_t new_size) {
  char *buffer = _buffer;

  size_t allocation_offset = 0;
  for (; tci_ecall(TCI_ECALL_IS_SAFE, buffer - 1 - allocation_offset);
       allocation_offset++)
    ;

  if (allocation_offset > 0)
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

void free(void *buffer) {}

void exit(int status) { tci_ecall(TCI_ECALL_EXIT, status); }

long strtol(const char *restrict str, char **restrict str_end, int base) {}

unsigned long strtoul(const char *restrict str, char **restrict str_end,
                      int base) {}
