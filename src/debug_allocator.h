#include <stddef.h>

// malloc fills its memory with this value before handing you a pointer to it
#define DEBUG_UNINIT 0xdadfaded
// realloc and free both set freed memory to this value before returning
#define DEBUG_FREED 0xdeadbeef
// The buffer memory of freed blocks gets set to this value by realloc and free
#define DEBUG_NEARBY_FREED 0xbadadded
// The buffer memory of allocated blocks gets set to this value by malloc
#define DEBUG_NEARBY_ALLOCATED 0xaabcdeff

#define malloc(size) __debug_alloc(size, __FILE__, __LINE__)
#define realloc(ptr, size) __debug_realloc(ptr, size, __FILE__, __LINE__)
#define free(ptr) __debug_dealloc(ptr, __FILE__, __LINE__)
#define check(ptr) (__debug_check_alloc(ptr, __FILE__, __LINE__), ptr)

// Replacement for malloc that tracks the file & line where it was called
void *__debug_alloc(size_t, char *, unsigned int);

// Replacement for realloc that tracks the file & line where it was called
void *__debug_realloc(void *, size_t, char *, unsigned int);

// Replacement for free that tracks the file & line where it was called
void __debug_dealloc(void *, char *, unsigned int);

// Checks the given pointer to see if it has been allocated already or not
void __debug_check_alloc(void *, char *, unsigned int);
