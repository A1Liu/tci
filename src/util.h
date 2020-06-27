
typedef char bool;
#define true 1
#define false 0

typedef struct {
  void *next;
  char *bump;
  uint64_t len;
} BumpList;

// if ptr != NULL, then ptr is the aligned bump pointer value to return, and
// next_bump is the next value of the bump pointer
typedef struct {
  void *ptr;
  void *next_bump;
} Bump;

// align must be a power of 2
Bump bump_ptr(void *bump_, void *end, uint64_t size) {
  char *bump = (char *)(((((size_t)bump_ - 1) >> 3) + 1) << 3);
  Bump result = {NULL, NULL};
  result.next_bump = bump + size;
  if (result.next_bump > end) {
    result.next_bump = NULL;
  } else
    result.ptr = bump;

  return result;
}

void *bump_alloc(BumpList *list, uint64_t size) {
  char *array_begin = (char *)(list + 1), *bucket_end = array_begin + list->len;

  Bump result = bump_ptr(list->bump, bucket_end, size);
  if (result.ptr != NULL) {
    list->bump = result.next_bump;
    return result.ptr;
  }

  if (list->next != NULL)
    return bump_alloc(list->next, size);

  uint64_t next_len = list->len / 2 + list->len;
  if (next_len < size)
    next_len = size;

  list->next = malloc(sizeof(*list) + next_len);

  BumpList *next = list->next;
  next->len = next_len;
  next->next = NULL;
  char *ptr = (char *)(next + 1);
  next->bump = ptr + size;

  return ptr;
}

BumpList *bump_new(void) {
  BumpList *list = malloc(sizeof(BumpList) + 1024);
  list->next = NULL;
  list->bump = (char *)(list + 1);
  list->len = 1024;
  return list;
}

typedef struct {
  char *begin;
  uint64_t end;
  uint64_t capacity;
} CharDynArray;

typedef struct {
  char *str;
  uint64_t len;
} String;

char CHAR_ARRAY[1024];

CharDynArray char_array_new(void) {
  CharDynArray arr = {NULL, 0, 0};
  return arr;
}

uint64_t char_array_add(CharDynArray *arr, char *buf, uint32_t len) {
  if (arr->begin == NULL) {
    arr->begin = malloc(256);
    arr->capacity = 256;
  }

  if (arr->capacity - arr->end < len) {
    arr->capacity = arr->capacity / 2 + arr->capacity + len;
    arr->begin = realloc(arr->begin, arr->capacity);
  }

  uint64_t begin = arr->end;
  for (int i = 0; i < len; i++, arr->end++) {
    arr->begin[arr->end] = buf[i];
  }

  return begin;
}

void char_array_finalize(CharDynArray *arr) {
  if (arr->capacity == arr->end)
    arr->begin = realloc(arr->begin, ++arr->capacity);
  arr->begin[arr->end++] = '\0';
}

uint64_t char_array_add_string(CharDynArray *arr, String str) {
  return char_array_add(arr, str.str, str.len);
}

String string_new(char *str) {
  String string = {str, strlen(str)};
  return string;
}

String string_from_parts(char *str, uint64_t len) {
  String s = {str, len};
  return s;
}

char *read_file(char *name) {
  FILE *file = fopen(name, "r");
  if (file == NULL)
    return NULL;

  CharDynArray arr = {NULL, 0, 0};

  char buf[256];
  size_t count;
  while ((count = fread(buf, 1, 256, file))) {
    char_array_add(&arr, buf, count);
  }

  fclose(file);
  char_array_finalize(&arr);
  return arr.begin;
}

bool streq(String s, char *val) {
  size_t len = strlen(val);
  if (s.len != len)
    return false;

  for (size_t i = 0; i < len; i++)
    if (s.str[i] != val[i])
      return false;

  return true;
}

String t_itoa(uint32_t val) {
  uint32_t length = snprintf(NULL, 0, "%u", val);
  snprintf(CHAR_ARRAY, length + 1, "%u", val);
  return string_from_parts(CHAR_ARRAY, length);
}
