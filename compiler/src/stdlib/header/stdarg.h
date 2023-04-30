#ifndef __TCI_STDARG_H
#define __TCI_STDARG_H

typedef struct __builtin_va_list {
  unsigned int __tci_va_current;
} va_list;

#define va_start(list, last) __builtin_va_start(list, last)
#define va_arg(list, type) (*(type *)(__builtin_va_arg(&list)))
#define va_end(list) __builtin_va_end(&list)

void *__builtin_va_arg(va_list *list);
void __builtin_va_end(va_list *list);

#endif
