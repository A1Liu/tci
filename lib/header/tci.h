#ifndef __TCI_TCI_H
#define __TCI_TCI_H
#include <stddef.h>

#define TCI_ECALL_EXIT 0
#define TCI_ECALL_ARGC 1
#define TCI_ECALL_ARGV 2

#define TCI_ECALL_PRINT_STRING 3

size_t tci_var_size(void *var);

void tci_throw_error(const char *name, const char *message,
                     unsigned int skip_frames);

void *tci_ecall(int ecall_num, ...);

#endif
