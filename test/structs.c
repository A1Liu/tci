struct FileId {
  int ident;
  int length;
  int cursor;
};

int ident_of(struct FileId file) { return file.ident; }

int main(int argc, char **argv) {
  struct FileId file;
  int id = ident_of(file);
}
