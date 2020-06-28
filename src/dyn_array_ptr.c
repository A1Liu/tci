#include "dyn_array_ptr.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

uint64_t *__dyn_array_capacity_ptr(void *arr) {
  char *capa_loc = ((char *)arr) - sizeof(uint64_t) * 2;
  return (uint64_t *)capa_loc;
}
uint64_t *__dyn_array_len_ptr(void *arr) {
  char *len_loc = ((char *)arr) - sizeof(uint64_t);
  return (uint64_t *)len_loc;
}

void __dyn_array_ensure_add(void *arr_, size_t size) {
  void **arr = (void **)arr_;
  void *buffer = *arr;

  if (buffer == NULL) {
    uint64_t *buffer_begin = malloc(size * 16);
    buffer = *arr = buffer_begin + 2;
    *__dyn_array_capacity_ptr(buffer) = 16;
    *__dyn_array_len_ptr(buffer) = 0;
    return;
  }

  uint64_t *buffer_begin = __dyn_array_capacity_ptr(buffer);
  uint64_t len = *__dyn_array_len_ptr(buffer);
  uint64_t capacity = *buffer_begin;
  if (len < capacity)
    return;

  capacity = capacity / 2 + capacity;
  buffer_begin = realloc(buffer_begin, size * capacity);
  *buffer_begin = capacity;
  *arr = buffer_begin + 2;
}

uint64_t __dyn_array_add_from(void *arr_, size_t size, void *from, size_t len) {
  void **arr = (void **)arr_;
  void *buffer = *arr;
  uint64_t *buffer_begin;

  if (buffer == NULL) {
    buffer_begin = malloc(size * (16 + len));
    buffer = *arr = buffer_begin + 2;
    *__dyn_array_capacity_ptr(buffer) = 16 + len;
    *__dyn_array_len_ptr(buffer) = len;
  } else {
    buffer_begin = __dyn_array_capacity_ptr(buffer);
    uint64_t array_len = *__dyn_array_len_ptr(buffer);
    uint64_t capacity = *buffer_begin;

    if (array_len + len >= capacity) {
      uint64_t capacity = *buffer_begin / 2 + *buffer_begin + len;
      buffer_begin = realloc(buffer_begin, size * capacity);
      *buffer_begin = capacity;
      buffer_begin[1] = array_len + len;
      buffer = *arr = buffer_begin + 2;
    }
  }
  memcpy(buffer, from, len * size);
  return *__dyn_array_len_ptr(buffer);
}
