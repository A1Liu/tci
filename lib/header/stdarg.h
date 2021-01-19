#ifndef __TCI_STDARG_H
#define __TCI_STDARG_H

typedef struct {
  void *current;
} va_list;

#define va_start(list, last) ((list).current = &(last), 0)
#define va_arg(list, type)                                                     \
  (*(type *)(list.current = ((char *)(list).current) - 1 - (long)(unsigned)-1))
#define va_end(list) ((list).current = 0)

#endif
