#ifndef __TCI_TCI_H
#define __TCI_TCI_H
#include <stddef.h>

#define TCI_ECALL_EXIT 0
#define TCI_ECALL_ARGC 1
#define TCI_ECALL_ARGV 2

#define TCI_ECALL_ALLOC_BEGIN 3
#define TCI_ECALL_ALLOC_END 4

#define TCI_ECALL_HEAP_ALLOC 5
#define TCI_ECALL_HEAP_DEALLOC 6

#define TCI_ECALL_THROW_ERROR 7
#define TCI_ECALL_PRINT_STRING 8

size_t tci_var_size(void *var);

void tci_throw_error(const char *name, const char *message,
                     unsigned int skip_frames);

void *tci_ecall(int ecall_num, ...);

#endif
