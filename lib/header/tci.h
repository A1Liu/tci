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

#define TCI_FILE_ERR_DOESNT_EXIST 1U
#define TCI_FILE_ERR_NAME_NOT_UTF8 2U
#define TCI_FILE_ERR_TOO_MANY_FILES 3U
#define TCI_FILE_ERR_FILES_TOO_LARGE 4U
#define TCI_FILE_ERR_OUT_OF_RANGE 5U

#define TCI_STDOUT_READ 6U
#define TCI_STDERR_READ 7U
#define TCI_STDLOG_READ 8U
#define TCI_STDIN_WRITE 9U

#define TCI_ERRNO_DOESNT_EXIST 4U
#define TCI_ERRNO_TOO_MANY_FILES 5U
#define TCI_ERRNO_FILES_TOO_LARGE 6U

size_t tci_var_size(void *var);

void tci_throw_error(const char *name, const char *message,
                     unsigned int skip_frames);

void *tci_ecall(int ecall_num, ...);

#endif
