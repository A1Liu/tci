#ifndef __TCI_TCI_H
#define __TCI_TCI_H

#define TCI_ECALL_EXIT 0U
#define TCI_ECALL_ARGC 1U
#define TCI_ECALL_ARGV 2U

#define TCI_ECALL_OPEN_FD 3U
#define TCI_ECALL_READ_FD 4U
#define TCI_ECALL_WRITE_FD 5U
#define TCI_ECALL_APPEND_FD 6U
#define TCI_ECALL_FD_LEN 7U

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

unsigned long tci_var_size(void *var);

void tci_throw_error(const char *name, const char *message,
                     unsigned int skip_frames);

void *tci_ecall(int ecall_num, ...);

void tci_assert_str(const char *str);

#define tci_assert_str(str)                                                    \
  (__tci_builtin_push(str), __tci_builtin_op("AssertStr", sizeof(void)))

void tci_perror(char *prefix, int error);

char *tci_strerror(int error);

#endif
