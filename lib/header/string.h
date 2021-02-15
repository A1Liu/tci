#ifndef __TCI_STRING_H
#define __TCI_STRING_H

typedef unsigned long size_t;

// Searches for the first occurrence of the character c (an unsigned char) in
// the first n bytes of the string pointed to, by the argument str.
void *memchr(const void *str, int c, size_t n);

// Compares the first n bytes of str1 and str2.
int memcmp(const void *str1, const void *str2, size_t n);

// Copies n characters from src to dest.
void *memcpy(void *dest, const void *src, size_t n);

// Another function to copy n characters from str2 to str1.
void *memmove(void *dest, const void *src, size_t n);

// Copies the character c (an unsigned char) to the first n characters of the
// string pointed to, by the argument str.
void *memset(void *str, int c, size_t n);

// Appends the string pointed to, by src to the end of the string pointed to by
// dest.
char *strcat(char *dest, const char *src);

// Appends the string pointed to, by src to the end of the string pointed to, by
// dest up to n characters long.
char *strncat(char *dest, const char *src, size_t n);

// Searches for the first occurrence of the character c (an unsigned char) in
// the string pointed to, by the argument str.
char *strchr(const char *str, int c);

// Compares the string pointed to, by str1 to the string pointed to by str2.
int strcmp(const char *str1, const char *str2);

// Compares at most the first n bytes of str1 and str2.
int strncmp(const char *str1, const char *str2, size_t n);

// Compares string str1 to str2. The result is dependent on the LC_COLLATE
// setting of the location.
int strcoll(const char *str1, const char *str2);

// Copies the string pointed to, by src to dest.
char *strcpy(char *dest, const char *src);

// Copies up to n characters from the string pointed to, by src to dest.
char *strncpy(char *dest, const char *src, size_t n);

// Calculates the length of the initial segment of str1 which consists entirely
// of characters not in str2.
size_t strcspn(const char *str1, const char *str2);

// Searches an internal array for the error number errnum and returns a pointer
// to an error message string.
char *strerror(int errnum);

// Computes the length of the string str up to but not including the terminating
// null character.
size_t strlen(const char *str);

// Finds the first character in the string str1 that matches any character
// specified in str2.
char *strpbrk(const char *str1, const char *str2);

// Searches for the last occurrence of the character c (an unsigned char) in the
// string pointed to by the argument str.
char *strrchr(const char *str, int c);

// Calculates the length of the initial segment of str1 which consists entirely
// of characters in str2.
size_t strspn(const char *str1, const char *str2);

// Finds the first occurrence of the entire string needle (not including the
// terminating null character) which appears in the string haystack.
char *strstr(const char *haystack, const char *needle);

// Breaks string str into a series of tokens separated by delim.
char *strtok(char *str, const char *delim);

// Transforms the first n characters of the string src into current locale and
// places them in the string dest.
size_t strxfrm(char *dest, const char *src, size_t n);

#endif
