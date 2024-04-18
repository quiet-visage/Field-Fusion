#pragma once

typedef struct {
    int code_index;
    float advance[2];
} ff_map_item_t;

typedef struct ht_codepoint_entry {
    int is_populated;
    int key;
    ff_map_item_t value;
    struct ht_codepoint_entry *next;
} ht_code_entry_t;

typedef struct {
    ff_map_item_t ext_ascii[0xff];
    ht_code_entry_t *hash_table;
} ff_map_t;

ff_map_t ff_map_create(void);
void ff_map_destroy(ff_map_t *m);
ff_map_item_t *ff_map_get(ff_map_t *m, int code);
ff_map_item_t *ff_map_insert(ff_map_t *m, int code);
