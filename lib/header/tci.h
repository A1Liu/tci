#ifndef __TCI_TCI_H
#define __TCI_TCI_H
#include <stddef.h>

#define TCI_ECALL_EXIT 0
#define TCI_ECALL_ARGC 1
#define TCI_ECALL_ARGV 2

#define TCI_ECALL_PRINT_STRING 3

#define TCI_ECALL_OPEN_FD 4
#define TCI_ECALL_READ_FD 5
#define TCI_ECALL_WRITE_FD 6
#define TCI_ECALL_APPEND_FD 7
#define TCI_ECALL_FD_LEN 8

size_t tci_var_size(void *var);

void tci_throw_error(const char *name, const char *message,
                     unsigned int skip_frames);

void *tci_ecall(int ecall_num, ...);

#endif
