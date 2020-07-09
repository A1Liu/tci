#include <limits.h>
#include <stdint.h>

typedef struct {
  uint32_t capa;
  uint32_t size;
  uint32_t *data;
} Hash;

Hash hash_new(uint32_t size, uint32_t data_type);
void *hash_find(Hash *, uint32_t key, uint32_t data_type);
void *hash_insert(Hash *, uint32_t key, uint32_t data_type);
void *hash_remove(Hash *, uint32_t key, uint32_t data_type);
