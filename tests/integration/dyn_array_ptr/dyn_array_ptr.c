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
    uint64_t *buffer_begin = malloc(size * 8 + sizeof(uint64_t) * 2);
    buffer = *arr = buffer_begin + 2;
    *__dyn_array_capacity_ptr(buffer) = 8;
    *__dyn_array_len_ptr(buffer) = 0;
    return;
  }

  uint64_t *buffer_begin = __dyn_array_capacity_ptr(buffer);
  uint64_t len = *__dyn_array_len_ptr(buffer);
  uint64_t capacity = *buffer_begin;
  if (len < capacity)
    return;

  capacity = capacity / 2 + capacity;
  buffer_begin = realloc(buffer_begin, size * capacity + sizeof(uint64_t) * 2);
  *buffer_begin = capacity;
  *arr = buffer_begin + 2;
}

uint64_t __dyn_array_add_from(void *arr_, size_t size, void *from, size_t len) {
  void **arr = (void **)arr_;
  char *buffer = *arr;
  uint64_t *buf_begin;
  uint64_t initial_len;

  if (buffer == NULL) {
    initial_len = 0;
    buf_begin = malloc(size * (8 + len) + sizeof(uint64_t) * 2);
    buffer = *arr = buf_begin + 2;
    *__dyn_array_capacity_ptr(buffer) = 8 + len;
    *__dyn_array_len_ptr(buffer) = len;
  } else {
    initial_len = *__dyn_array_len_ptr(buffer);
    buf_begin = __dyn_array_capacity_ptr(buffer);
    uint64_t capacity = *buf_begin;

    if (initial_len + len >= capacity) {
      uint64_t capa = capacity / 2 + capacity + len;
      buf_begin = realloc(buf_begin, size * capa + sizeof(uint64_t) * 2);
      buffer = *arr = buf_begin + 2;
      *__dyn_array_capacity_ptr(buffer) = capa;
    }

    *__dyn_array_len_ptr(buffer) = initial_len + len;
  }

  memcpy(buffer + size * initial_len, from, size * len);
  return initial_len;
}
