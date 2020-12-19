#include <stddef.h>

#define TCI_ECALL_EXIT 0
#define TCI_ECALL_ARGC 1
#define TCI_ECALL_ARGV 2
#define TCI_ECALL_IS_SAFE 3
#define TCI_ECALL_HEAP_ALLOC 4
#define TCI_ECALL_THROW_ERROR 5

size_t tci_var_size(void *var);

#define tci_throw_error(name, message)                                         \
  tci_ecall(TCI_ECALL_THROW_ERROR, name, message, 1)

void *tci_ecall(int ecall_num, ...);
