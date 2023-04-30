#include "tree.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

NODE *root;

// Function to implement strcpy() function
char *strcpy(char *destination, const char *source) {
  // return if no memory is allocated to the destination
  if (destination == NULL)
    return NULL;

  // take a pointer pointing to the beginning of destination string
  char *ptr = destination;

  // copy the C-string pointed by source into the array
  // pointed by destination
  while (*source != '\0') {
    *destination = *source;
    destination++;
    source++;
  }

  // include the terminating null character
  *destination = '\0';

  // destination is returned by standard strcpy()
  return ptr;
}

void rec_insert_node(NODE *n, NODE *r) {
  if (strcmp((n->word), (r->word)) < 0) {
    if (r->left == NULL) {
      r->left = n;
      r->right = NULL;
    } else {
      rec_insert_node(n, r->left);
    }
  } else {
    if ((n->right) == NULL) {
      r->right = n;
      r->left = NULL;
    } else {
      rec_insert_node(n, r->right);
    }
  }
}

void insert_node(char *string) {
  NODE *newnode = (NODE *)malloc(sizeof(NODE));
  newnode->word = string;
  if (root != NULL) {
    rec_insert_node(newnode, root);
  } else {
    root->left = newnode;
    newnode->right = root;
    root->right = NULL;
  }
}

void print_tree(NODE *r) {
  if (r == NULL) {
    return;
  } else {
    print_tree(r->left);
    printf("%s", r->word);
    print_tree(r->right);
  }
}
