#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <tci.h>

// adapted from https://github.com/mpaland/printf

int printf(char *format, ...) {
  va_list list;
  va_start(list, format);
  tci_ecall(TCI_ECALL_PRINTF, format, list);
  va_end(list);
  return 0;
}

int scanf(char *format, ...) {
  tci_ecall(TCI_ECALL_EXIT, 1);
  return 0;
}
