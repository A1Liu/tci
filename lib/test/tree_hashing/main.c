#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SIZE 100

typedef struct hashcell {
  char *word;
  struct hashcell *next;

} HASHCELL;

HASHCELL *hashtable[SIZE] = {NULL};

unsigned int hash_string(char *string) {
  unsigned int hash = 1;
  for (int i = 0; i < strlen(string); i++) {
    char c = string[i];
    hash = (hash * 7) + c;
  }
  return hash % SIZE;
}

void insert_hash_cell(char *string) {

  unsigned int index = hash_string(string);
  HASHCELL *c = (HASHCELL *)malloc(sizeof(HASHCELL));
  c->word = (char *)malloc(strlen(string) + 1);

  strcpy(c->word, string);
  int a;
  for (int i = 0; i < SIZE; i++) {
    a = strcmp(hashtable[i]->word, c->word);
    if (a == 0) {
      break;
    }
  }
  if (a != 0) {
    hashtable[index] = c;
    hashtable[index]->next = NULL;
  }
}

void print_hash_table() {
  for (int i = 0; i < SIZE; i++) {
    if (hashtable[i]->word != NULL) {
      printf("i: %s\n", hashtable[i]->word);
    }
  }
}

int main() {
  char str[100];
  while (scanf(" %s", str) != EOF) {
    printf(" %s", str);

    insert_hash_cell(str);
  }

  print_hash_table();
}
