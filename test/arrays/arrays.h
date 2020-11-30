#define INLINE_LEN 4

typedef struct {
  char data[];
} * StringData;

typedef struct {
  int size;
  char _inline[INLINE_LEN];
  StringData data;
} String;

String string_new(char *);
char *string_data(String);
