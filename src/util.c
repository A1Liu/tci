typedef struct {
  void *next;
  char *bump;
  size_t len;
} BucketList;

// if ptr != NULL, then ptr is the aligned bump pointer value to return, and
// next_bump is the next value of the bump pointer
typedef struct {
  void *ptr;
  void *next_bump;
} Bump;

// align must be a power of 2
Bump bump_ptr(void *bump_, void *end, size_t size) {
  char *bump = (char *)(((((size_t)bump_ - 1) >> 3) + 1) << 3);
  Bump result = {NULL, NULL};
  result.next_bump = bump + size;
  if (result.next_bump > end) {
    result.next_bump = NULL;
  } else
    result.ptr = bump;

  return result;
}

void *alloc(BucketList *list, size_t size) {
  char *array_begin = (char *)(list + 1), *bucket_end = array_begin + list->len;

  Bump result = bump_ptr(list->bump, bucket_end, size);
  if (result.ptr != NULL) {
    list->bump = result.next_bump;
    return result.ptr;
  }

  if (list->next != NULL)
    return alloc(list->next, size);

  size_t next_len = list->len / 2 + list->len;
  if (next_len < size)
    next_len = size;

  list->next = malloc(sizeof(*list) + next_len);

  BucketList *next = list->next;
  next->len = next_len;
  next->next = NULL;
  char *ptr = (char *)(next + 1);
  next->bump = ptr + size;

  return ptr;
}

typedef struct {
  char *begin;
  size_t end;
  size_t capacity;
} StringDynArray;

void char_array_add(StringDynArray *arr, char *buf, size_t len) {
  if (arr->begin == NULL) {
    arr->begin = malloc(256);
    arr->capacity = 256;
  }

  if (arr->capacity - arr->end < len) {
    arr->capacity = arr->capacity / 2 + arr->capacity + len;
    arr->begin = realloc(arr->begin, arr->capacity);
  }

  for (int i = 0; i < len; i++, arr->end++) {
    arr->begin[arr->end] = buf[i];
  }
}

void finalize(StringDynArray *arr) {
  if (arr->capacity == arr->end)
    arr->begin = realloc(arr->begin, ++arr->capacity);
  arr->begin[arr->end++] = '\0';
}

typedef struct {
  char *str;
  size_t len;
} String;

char *read_file(char *name) {
  FILE *file = fopen(name, "r");
  if (file == NULL)
    return NULL;

  StringDynArray arr = {NULL, 0, 0};

  char buf[256];
  size_t count;
  while ((count = fread(buf, 1, 256, file))) {
    char_array_add(&arr, buf, count);
  }
  fclose(file);
  finalize(&arr);
  return arr.begin;
}
