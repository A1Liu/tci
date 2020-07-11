typedef struct {
  Range range;
  String message;
} ErrorMessage;

typedef struct {
  String message;
  ErrorMessage *highlights;
} Error;

Error error_new(String message) {
  Error err = {message, NULL};
  return err;
}

void error_array_add(Error *err, Range code_loc, String message) {
  ErrorMessage err_msg = {code_loc, message};
  dyn_array_add(&err->highlights, err_msg);
}

String error_str(char **arr, Error err) {
  uint64_t begin = char_array_add_string(arr, string_new("ERROR: "));
  char_array_add_string(arr, err.message);
  dyn_array_add(arr, '\n');
  dyn_array_add(arr, '\n');

  uint64_t hl_count = dyn_array_len(err.highlights);
  for (uint64_t i = 0; i < hl_count; i++) {
    ErrorMessage *hl = err.highlights + i;

    char_array_add_string(arr, t_itoa(hl->range.begin));

    dyn_array_add(arr, ',');
    char_array_add_string(arr, t_itoa(hl->range.end));
    char_array_add_string(arr, string_new(": "));

    char_array_add_string(arr, hl->message);
    dyn_array_add(arr, '\n');
  }

  return string_from_parts(&(*arr)[begin], dyn_array_len(*arr) - begin);
}
