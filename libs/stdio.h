#include <stdio.h>
#include <tci.h>

int scanf(char *format, ...) {
  tci_ecall(TCI_ECALL_EXIT, 1);
  return 0;
}
