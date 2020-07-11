#include "hashtable.h"
#include <stdlib.h>
#include <string.h>

#define NULL_KEY INT_MAX - 1
#define TOMBSTONE_KEY INT_MAX

typedef struct {
  uint32_t key;
  uint32_t value;
} Item;

static uint32_t hashword(uint32_t c) {
  // Bob Jenkin's mix function
  uint32_t a, b;
  a = b = 0x9e3779b9; /* the golden ratio; an arbitrary value */
  a -= b;
  a -= c;
  a ^= (c >> 13);
  b -= c;
  b -= a;
  b ^= (a << 8);
  c -= a;
  c -= b;
  c ^= (b >> 13);
  a -= b;
  a -= c;
  a ^= (c >> 12);
  b -= c;
  b -= a;
  b ^= (a << 16);
  c -= a;
  c -= b;
  c ^= (b >> 5);
  a -= b;
  a -= c;
  a ^= (c >> 3);
  b -= c;
  b -= a;
  b ^= (a << 10);
  c -= a;
  c -= b;
  c ^= (b >> 15);
  return c;
}

// https://stackoverflow.com/a/365068
static inline uint32_t pow2roundup(uint32_t x) {
  x--;
  x |= x >> 1;
  x |= x >> 2;
  x |= x >> 4;
  x |= x >> 8;
  x |= x >> 16;
  return x + 1;
}

Hash hash_new(uint32_t size, uint32_t data_type) {
  if (size < 8)
    size = 8;
  if (data_type < sizeof(uint32_t))
    data_type = 0;

  Hash hash;
  hash.capa = pow2roundup(size);
  hash.size = 0;
  hash.data = malloc(hash.capa * sizeof(Item) + data_type * (hash.capa >> 1));
  uint32_t capa2 = hash.capa * 2;
  for (uint32_t i = 0; i < capa2; i += 2) {
    hash.data[i] = NULL_KEY;
  }

  return hash;
}

static void *hash_value_for_idx(Hash *hash, uint32_t idx, uint32_t data_type) {
  if (data_type <= sizeof(uint32_t))
    return hash->data + 2 * idx + 1;
  char *data_begin = (void *)(hash->data + hash->capa);
  return data_begin + hash->data[idx + 1] * data_type;
}

static void hash_grow(Hash *hash, uint32_t data_type) {
  if (data_type <= sizeof(uint32_t))
    data_type = sizeof(uint32_t);

  uint32_t capa2 = hash->capa * 2;
  uint32_t *data = hash->data;
  Hash h2 = hash_new(capa2, data_type);

  for (uint32_t i = 0; i < hash->capa; i++) {
    uint32_t i2 = i * 2;

    if (data[i2] == NULL_KEY || data[i2] == TOMBSTONE_KEY)
      continue;

    void *value = hash_insert(&h2, data[i2], data_type);
    memcpy(value, hash_value_for_idx(hash, i, data_type), data_type);
  }

  *hash = h2;
}

static uint32_t hash_idx_for_key(Hash *hash, uint32_t key) {
  uint32_t *data = hash->data;
  uint32_t idx = 2 * (hashword(key) & (hash->capa - 1));
  uint32_t capa2 = hash->capa * 2;

  for (; data[idx] != NULL_KEY && data[idx] != key;
       idx += 2, idx = idx >= capa2 ? 0 : idx)
    ;

  return idx;
}

void *hash_find(Hash *hash, uint32_t key, uint32_t data_type) {
  uint32_t idx = hash_idx_for_key(hash, key);
  if (hash->data[idx] == NULL_KEY)
    return NULL;

  return hash_value_for_idx(hash, idx, data_type);
}

void *hash_insert(Hash *hash, uint32_t key, uint32_t data_type) {
  if (hash->size * 2 >= hash->capa)
    hash_grow(hash, data_type);

  uint32_t *data = hash->data;
  uint32_t idx = 2 * (hashword(key) & (hash->capa - 1));
  uint32_t capa2 = hash->capa * 2;

  for (;
       data[idx] != NULL_KEY && data[idx] != key && data[idx] != TOMBSTONE_KEY;
       idx += 2, idx = idx >= capa2 ? 0 : idx)
    ;

  data[idx] = key;
  data[idx + 1] = hash->size++;
  return hash_value_for_idx(hash, idx, data_type);
}

void *hash_remove(Hash *hash, uint32_t key, uint32_t data_type) {
  uint32_t idx = hash_idx_for_key(hash, key);
  if (hash->data[idx] == NULL_KEY)
    return NULL;

  hash->data[idx] = TOMBSTONE_KEY;
  return hash_value_for_idx(hash, idx, data_type);
}
