#include <stddef.h>
#include <stdint.h>

#define dyn_array_declare(name, type) type *name = (type *)0;
#define dyn_array_new(type) ((type *)0)
#define dyn_array_capacity(arr) *__dyn_array_capacity_ptr(arr)
#define dyn_array_len(arr) *__dyn_array_len_ptr(arr)

#define dyn_array_add(arr, elem)                                               \
  (__dyn_array_ensure_add(arr, sizeof(**arr)),                                 \
   (*arr)[*__dyn_array_len_ptr(*arr)] = elem,                                  \
   *__dyn_array_len_ptr(*arr) = *__dyn_array_len_ptr(*arr) + 1,                \
   *__dyn_array_len_ptr(*arr) - 1)

#define dyn_array_add_from(arr, elems, len)                                    \
  (/*ELEMENTS_ARE_WRONG_TYPE*/ elems - *arr,                                   \
   __dyn_array_add_from(arr, sizeof(**arr), elems, len))

uint64_t *__dyn_array_capacity_ptr(void *);
uint64_t *__dyn_array_len_ptr(void *);
void __dyn_array_ensure_add(void *, size_t);
uint64_t __dyn_array_add_from(void *, size_t, void *, size_t);
