#include "code_map.h"

#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#define HASH_TABLE_SIZE 0x200

static unsigned int int_hash(int key) {
    size_t value = ((key >> 16) ^ key) * 0x45d9f3b;
    value = ((value >> 16) ^ value) * 0x45d9f3b;
    value = (value >> 16) ^ value;

    return value;
}

FF_Map ff_map_create(void) {
    return (FF_Map){.ext_ascii = {},
                      .hash_table = calloc(HASH_TABLE_SIZE,
                                           sizeof(FF_Code_Entry))};
}

FF_Map_Item *ff_map_get(FF_Map *m, int code) {
    if (code >= 0 && code < 0xff) {
        return &m->ext_ascii[code];
    }

    size_t slot = int_hash(code) % HASH_TABLE_SIZE;
    FF_Code_Entry *entry = &m->hash_table[slot];
    if (!entry->is_populated) return 0;

    FF_Code_Entry *head = entry;
    while (head != NULL) {
        if (head->key == code) {
            return &head->value;
        }
        head = head->next;
    }

    return 0;
}

FF_Map_Item *ff_map_insert(FF_Map *m, int code) {
    FF_Map_Item *result = 0;
    if (code >= 0 && code < 0xff) {
        m->ext_ascii[code].code_index = code;
        result = &m->ext_ascii[code];
        return result;
    }

    FF_Map_Item map_item_val = {0};
    FF_Code_Entry slot_entry_val = {.is_populated = 1,
                                      .key = code,
                                      .value = map_item_val,
                                      .next = 0};

    size_t slot = int_hash(code) % HASH_TABLE_SIZE;
    FF_Code_Entry *entry = &m->hash_table[slot];
    if (!entry->is_populated) {
        m->hash_table[slot] = slot_entry_val;
        return &entry->value;
    }

    FF_Code_Entry *prev = 0;
    FF_Code_Entry *head = entry;
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
        (FF_Code_Entry *)calloc(1, sizeof(FF_Code_Entry));
    memcpy(prev->next, &slot_entry_val,
           sizeof(FF_Code_Entry));

    return &prev->next->value;
}

static void entry_destroy(FF_Code_Entry *entry) {
    if (!entry->is_populated) return;
    if (!entry->next) return;
    entry_destroy(entry->next);
    free(entry->next);
}

void ff_map_destroy(FF_Map *m) {
    for (size_t i = 0; i < HASH_TABLE_SIZE; i += 1) {
        FF_Code_Entry *entry = &m->hash_table[i];
        if (!entry->is_populated || !entry->next) continue;
        entry_destroy(entry);
    }
    free(m->hash_table);
}
