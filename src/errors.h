typedef struct {
  uint32_t begin;
  uint32_t len;
} Range;

typedef struct {
  Range range;
  String message;
} ErrorMessage;

typedef struct {
  String message;
  ErrorMessage *highlights;
} Error;

Range range_new(uint32_t begin, uint32_t len) {
  Range r = {begin, len};
  return r;
}

Error error_new(String message) {
  Error err = {message, NULL};
  return err;
}

void error_array_add(Error *err, Range code_loc, String message) {
  ErrorMessage err_msg = {code_loc, message};
  dyn_array_add(&err->highlights, err_msg);
}
