#include "code_map.h"
#define HASH_TABLE_SIZE 0x200

static unsigned int int_hash(int key) {
    ulong value = ((key >> 16) ^ key) * 0x45d9f3b;
    value = ((value >> 16) ^ value) * 0x45d9f3b;
    value = (value >> 16) ^ value;

    return value;
}

ff_map_t ff_map_create(void) {
    return (ff_map_t){.ext_ascii = {},
                      .hash_table = calloc(HASH_TABLE_SIZE,
                                           sizeof(ht_code_entry_t))};
}

ff_map_item_t *ff_map_get(ff_map_t *m, int code) {
    if (code >= 0 && code < 0xff) {
        return &m->ext_ascii[code];
    }

    size_t slot = int_hash(code) % HASH_TABLE_SIZE;
    ht_code_entry_t *entry = &m->hash_table[slot];
    if (!entry->is_populated) return 0;

    ht_code_entry_t *head = entry;
    while (head != NULL) {
        if (head->key == code) {
            return &head->value;
        }
        head = head->next;
    }

    return 0;
}

ff_map_item_t *ff_map_insert(ff_map_t *m, int code) {
    ff_map_item_t *result = 0;
    if (code >= 0 && code < 0xff) {
        m->ext_ascii[code].code_index = code;
        result = &m->ext_ascii[code];
        return result;
    }

    ff_map_item_t map_item_val = {0};
    ht_code_entry_t slot_entry_val = {.is_populated = 1,
                                      .key = code,
                                      .value = map_item_val,
                                      .next = 0};

    size_t slot = int_hash(code) % HASH_TABLE_SIZE;
    ht_code_entry_t *entry = &m->hash_table[slot];
    if (!entry->is_populated) {
        m->hash_table[slot] = slot_entry_val;
        return &entry->value;
    }

    ht_code_entry_t *prev = 0;
    ht_code_entry_t *head = entry;
    while (head != NULL) {
        if (head->key == code) {
            entry->value = map_item_val;
            return &entry->value;
        }

        // walk to next
        prev = head;
        head = prev->next;
    }
    prev->next =
        (ht_code_entry_t *)calloc(1, sizeof(ht_code_entry_t));
    memcpy(prev->next, &slot_entry_val,
           sizeof(struct ht_codepoint_entry));

    return &prev->next->value;
}

static void entry_destroy(ht_code_entry_t *entry) {
    if (!entry->is_populated) return;
    if (!entry->next) return;
    entry_destroy(entry->next);
    free(entry->next);
}

void ff_map_destroy(ff_map_t *m) {
    for (size_t i = 0; i < HASH_TABLE_SIZE; i += 1) {
        ht_code_entry_t *entry = &m->hash_table[i];
        if (!entry->is_populated || !entry->next) continue;
        entry_destroy(entry);
    }
    free(m->hash_table);
}
