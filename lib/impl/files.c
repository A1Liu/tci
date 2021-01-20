#include <stdio.h>
#include <tci.h>

char *__tci_stdout_buffer[BUFSIZ];
FILE __tci_stdout_struct = {__tci_stdout_buffer, 0, BUFSIZ, 0, 0, 1, 0, 0};
char *__tci_stderr_buffer[BUFSIZ];
FILE __tci_stderr_struct = {__tci_stderr_buffer, 0, BUFSIZ, 0, 0, 2, 0, 0};

FILE *__tci_stdout = &__tci_stdout_struct;
FILE *__tci_stderr = &__tci_stderr_struct;
FILE *__tci_stdin;

FILE *fopen(const char *filename, const char *mode);
int fclose(FILE *fp);

int fputc(int c, FILE *fp);
int fputs(const char *s, FILE *fp);

int fflush(FILE *fp);

int fgetc(FILE *fp);
char *fgets(char *buf, int n, FILE *fp);

size_t fread(void *ptr, size_t size_of_elements, size_t number_of_elements,
             FILE *a_file);
size_t fwrite(const void *ptr, size_t size_of_elements,
              size_t number_of_elements, FILE *a_file);
