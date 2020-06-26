#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef char bool;
#define false 0
#define true 1
#define DEBUG false
#define RELEASE false

typedef struct {
  void *begin;
  size_t len;
  char *malloc_file;
  unsigned int malloc_line;
  unsigned int free_line;
  char *free_file;
  bool valid;
} AllocInfo;

typedef struct {
  AllocInfo *begin;
  size_t end;
  size_t capacity;
} AllocVec;

static AllocVec alloc_info = {NULL, 0, 0};

bool __alloc_info_check_ptr(AllocInfo *info, void *ptr, char *file,
                            unsigned int line) {
  void *buffered_begin = info->begin - info->len * 2;
  if (ptr < buffered_begin || ptr - buffered_begin >= info->len * 5)
    return false;

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
    fprintf(stderr,
            "%s:%u: checking pointer at 0x%lx FAILED (malloc at %s:%u, free at "
            "%s:%u)\n",
            file, line, (size_t)ptr, info->malloc_file, info->malloc_line,
            info->free_file, info->free_line);
    exit(1);
  }

  return true;
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

void __alloc_info_free(AllocInfo *info, char *file, unsigned int line) {
  info->valid = false;
  info->free_file = file;
  info->free_line = line;
}

void *__debug_alloc(size_t size, char *file, unsigned int line) {
  if (DEBUG)
    fprintf(stderr, "%s:%u: allocating block of size %lu...", file, line, size);
  char *allocation = malloc(size * 5) + size * 2;
  if (DEBUG)
    fprintf(stderr, "got 0x%lx\n", (size_t)allocation);
  __alloc_vec_append(allocation, size, file, line);
  return allocation;
}

void *__debug_realloc(void *ptr, size_t size, char *file, unsigned int line) {
  if (ptr == NULL)
    return __debug_alloc(size, file, line);

  if (alloc_info.begin == NULL)
    fprintf(stderr,
            "%s:%u: realloc'ing pointer at 0x%lx FAILED (no "
            "allocations have been performed yet)\n",
            file, line, (size_t)ptr);

  for (size_t i = alloc_info.end - 1; i != -1; i--) {
    AllocInfo *info = &alloc_info.begin[i];
    if (!__alloc_info_check_ptr(info, ptr, file, line))
      continue;

    if (ptr != info->begin) {
      fprintf(stderr,
              "%s:%u: checking pointer at 0x%lx FAILED (malloc at %s:%u, "
              "realloc called on 0x%lx, should've been called on 0x%lx)\n",
              file, line, (size_t)ptr, info->malloc_file, info->malloc_line,
              (size_t)ptr, (size_t)info->begin);
      exit(1);
    }

    __alloc_info_free(info, file, line);

    char *buffer_begin = ((char *)ptr) - info->len * 2;
    void *allocation = malloc(size * 5) + size * 2;
    memcpy(allocation, ptr, info->len);
    free(buffer_begin);
    __alloc_vec_append(allocation, size, file, line);

    if (DEBUG)
      fprintf(stderr, "%s:%u: realloc'ing pointer at 0x%lx...SUCCESS\n", file,
              line, (size_t)ptr);

    return allocation;
  }

  fprintf(
      stderr,
      "%s:%u: realloc'ing pointer at 0x%lx...FAILED (coulnd't find pointer)\n",
      file, line, (size_t)ptr);
  exit(1);
}

void __debug_dealloc(void *ptr, char *file, unsigned int line) {
  if (alloc_info.begin == NULL)
    fprintf(stderr,
            "%s:%u: freeing pointer at 0x%lx FAILED (no allocations have been "
            "performed yet)\n",
            file, line, (size_t)ptr);

  for (size_t i = alloc_info.end - 1; i != -1; i--) {
    AllocInfo *info = &alloc_info.begin[i];
    if (!__alloc_info_check_ptr(info, ptr, file, line))
      continue;

    if (ptr != info->begin) {
      fprintf(stderr,
              "%s:%u: checking pointer at 0x%lx FAILED (malloc at %s:%u, "
              "free called on 0x%lx, should've been called on 0x%lx)\n",
              file, line, (size_t)ptr, info->malloc_file, info->malloc_line,
              (size_t)ptr, (size_t)info->begin);
      exit(1);
    }

    __alloc_info_free(info, file, line);
    free(((char *)ptr) - info->len * 2);
    fprintf(stderr, "%s:%u: deallocating pointer at 0x%lx...SUCCESS\n", file,
            line, (size_t)ptr);
    return;
  }

  fprintf(stderr,
          "%s:%u: freeing pointer at 0x%lx...FAILED (couldn't find pointer)\n",
          file, line, (size_t)ptr);
  exit(1);
}

void __debug_check_alloc(void *ptr, char *file, unsigned int line) {
  if (RELEASE)
    return;

  for (size_t i = alloc_info.end - 1; i != -1; i--) {
    AllocInfo *info = &alloc_info.begin[i];
    if (!__alloc_info_check_ptr(info, ptr, file, line))
      continue;

    if (DEBUG)
      fprintf(stderr, "%s:%u: checking pointer at 0x%lx...SUCCESS", file, line,
              (size_t)ptr);
    return;
  }

  fprintf(stderr,
          "%s:%u: checking pointer at 0x%lx...FAILED (couldn't find pointer)\n",
          file, line, (size_t)ptr);
  exit(1);
}

#define malloc(size) __debug_alloc(size, __FILE__, __LINE__)
#define realloc(ptr, size) __debug_realloc(ptr, size, __FILE__, __LINE__)
#define free(ptr) __debug_dealloc(ptr, __FILE__, __LINE__)
#define check(ptr) (__debug_check_alloc(ptr, __FILE__, __LINE__), ptr)
