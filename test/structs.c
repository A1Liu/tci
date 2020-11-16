struct FileId {
  int ident;
  struct Location {
    int length;
    int cursor;
  } location;
};

int printf(char *format, ...);
int ident_of(struct FileId file) { return file.ident; }
struct Location location_of(struct FileId *file) {
  return file->location;
}

int main() {
  struct FileId file;
  struct FileId file2;
  struct FileId *file3;
  file3 = &file2;
  file.ident = 12;
  file2.ident = ident_of(file);

  printf("%d\n", file3->ident);
  printf("sizeof FileId is %ld\n", sizeof(struct FileId));
  printf("sizeof file3.ident is %ld\n", sizeof file3.ident);
  printf("location is {%d, %d}\n", location_of(&file2).length,
         location_of(&file2).cursor);
  file3->ident = 13;
  printf("%d\n", file2);
}
