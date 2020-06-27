#include "debug_allocator.h"
#undef malloc
#undef free
#undef realloc
#undef check

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef char bool;
#define false 0
#define true 1
#define DEBUG false

typedef struct {
  void *begin;
  size_t len;
  char *malloc_file;
  unsigned int malloc_line;
  unsigned int free_line;
  bool valid;
  char *free_file;
} AllocInfo;

typedef struct {
  AllocInfo *begin;
  size_t end;
  size_t capacity;
} AllocVec;

static AllocVec alloc_info = {NULL, 0, 0};

AllocInfo *__alloc_vec_check_ptr(void *ptr, char *file, unsigned int line) {
  if (alloc_info.begin == NULL) {
    fprintf(stderr,
            "%s:%u: checking pointer at 0x%lx FAILED (no allocations have been "
            "performed yet)\n",
            file, line, (size_t)ptr);
    exit(1);
  }

  for (size_t i = alloc_info.end - 1; i != -1; i--) {
    AllocInfo *info = &alloc_info.begin[i];
    size_t buffered_len = ((info->len - 1) & ~7) + 8;
    void *buffered_begin = info->begin - buffered_len * 2;
    if (ptr < buffered_begin || ptr - buffered_begin >= info->len * 5)
      continue;

    if (ptr < info->begin || ptr - info->begin >= info->len) {
      if (info->valid)
        fprintf(stderr,
                "%s:%u: checking pointer at 0x%lx FAILED (nearby malloc from "
                "%s:%u at 0x%lx)\n",
                file, line, (size_t)ptr, info->malloc_file, info->malloc_line,
                (size_t)info->begin);
      else
        fprintf(stderr,
                "%s:%u: checking pointer at 0x%lx FAILED (nearby malloc from "
                "%s:%u at 0x%lx, free from %s:%u)\n",
                file, line, (size_t)ptr, info->malloc_file, info->malloc_line,
                (size_t)info->begin, info->free_file, info->free_line);
      exit(1);
    }

    if (!info->valid) {
      fprintf(
          stderr,
          "%s:%u: checking pointer at 0x%lx FAILED (malloc at %s:%u, free at "
          "%s:%u)\n",
          file, line, (size_t)ptr, info->malloc_file, info->malloc_line,
          info->free_file, info->free_line);
      exit(1);
    }

    return info;
  }

  return NULL;
}

void __alloc_vec_append(void *ptr, size_t size, char *file, unsigned int line) {
  if (alloc_info.begin == NULL) {
    alloc_info.begin = malloc(sizeof(AllocInfo) * 16);
    alloc_info.capacity = 16;
  }

  if (alloc_info.end == alloc_info.capacity) {
    alloc_info.capacity = alloc_info.capacity * 2;
    size_t new_size = sizeof(AllocInfo) * alloc_info.capacity;
    alloc_info.begin = realloc(alloc_info.begin, new_size);
  }

  AllocInfo *info = &alloc_info.begin[alloc_info.end++];
  info->begin = ptr;
  info->len = size;
  info->valid = true;
  info->malloc_line = line;
  info->malloc_file = file;
  info->free_file = NULL;
  info->free_line = 0;
}

void __debug_fill_region(uint64_t *ptr, size_t size, uint64_t value) {
  size_t blocs = size / 8;
  for (size_t i = 0; i < blocs; i++)
    ptr[i] = value;
}

void __alloc_info_free(AllocInfo *info, char *file, unsigned int line) {
  info->valid = false;
  info->free_file = file;
  info->free_line = line;

  size_t buffered_len = ((info->len - 1) & ~7) + 8;
  void *buffer_begin = ((char *)info->begin) - buffered_len * 2;
  void *allocation_end = ((char *)info->begin) + buffered_len;
  __debug_fill_region(buffer_begin, buffered_len * 2, DEBUG_NEARBY_FREED);
  __debug_fill_region(info->begin, buffered_len, DEBUG_FREED);
  __debug_fill_region(allocation_end, buffered_len * 2, DEBUG_NEARBY_FREED);
  free(buffer_begin);
}

void *__debug_alloc(size_t size, char *file, unsigned int line) {
  if (size == 0)
    fprintf(stderr, "%s:%u: WARN malloc'ing a block of size 0\n", file, line);

  size = ((size - 1) & ~7) + 8;
  void *buffer_begin = malloc(size * 5);
  void *allocation = buffer_begin + size * 2;
  void *allocation_end = ((char *)allocation) + size;

  if (DEBUG)
    fprintf(stderr, "%s:%u: requested block of size %lu, got 0x%lx\n", file,
            line, size, (size_t)allocation);

  __debug_fill_region(buffer_begin, size * 2, DEBUG_NEARBY_ALLOCATED);
  __debug_fill_region(allocation, size, DEBUG_UNINIT);
  __debug_fill_region(allocation_end, size * 2, DEBUG_NEARBY_ALLOCATED);
  __alloc_vec_append(allocation, size, file, line);
  return allocation;
}

void *__debug_realloc(void *ptr, size_t size, char *file, unsigned int line) {
  void *allocation = __debug_alloc(size, file, line);
  if (ptr == NULL)
    return allocation;

  AllocInfo *info = __alloc_vec_check_ptr(ptr, file, line);
  if (!info) {
    fprintf(
        stderr,
        "%s:%u: realloc'ing pointer at 0x%lx FAILED (coulnd't find pointer)\n",
        file, line, (size_t)ptr);
    exit(1);
  }

  if (info->len < size)
    memcpy(allocation, ptr, info->len);
  else
    memcpy(allocation, ptr, size);

  __alloc_info_free(info, file, line);
  return allocation;
}

void __debug_dealloc(void *ptr, char *file, unsigned int line) {
  AllocInfo *info = __alloc_vec_check_ptr(ptr, file, line);
  if (!info) {
    fprintf(stderr,
            "%s:%u: freeing pointer at 0x%lx FAILED (couldn't find pointer)\n",
            file, line, (size_t)ptr);
    exit(1);
  }

  if (ptr != info->begin) {
    fprintf(stderr,
            "%s:%u: checking pointer at 0x%lx FAILED (malloc at %s:%u, "
            "free called on 0x%lx, should've been called on 0x%lx)\n",
            file, line, (size_t)ptr, info->malloc_file, info->malloc_line,
            (size_t)ptr, (size_t)info->begin);
    exit(1);
  }

  __alloc_info_free(info, file, line);
  if (DEBUG)
    fprintf(stderr, "%s:%u: deallocating pointer at 0x%lx SUCCESS\n", file,
            line, (size_t)ptr);
}

void __debug_check_alloc(void *ptr, char *file, unsigned int line) {
  AllocInfo *info = __alloc_vec_check_ptr(ptr, file, line);
  if (!info) {
    fprintf(stderr,
            "%s:%u: checking pointer at 0x%lx FAILED (couldn't find pointer)\n",
            file, line, (size_t)ptr);
    exit(1);
  }

  if (DEBUG)
    fprintf(stderr, "%s:%u: checking pointer at 0x%lx SUCCESS\n", file, line,
            (size_t)ptr);
}
