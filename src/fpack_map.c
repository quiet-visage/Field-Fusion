#include "fpack_map.h"
#define HT_FPACK_TABLE_SIZE 0x200

#define ENTRY_IS_EMPTY(entry) (!(entry).not_empty)

static inline int int_hash(int key) {
    ulong value = ((key >> 16) ^ key) * 0x45d9f3b;
    value = ((value >> 16) ^ value) * 0x45d9f3b;
    value = (value >> 16) ^ value;
    return value;
}

void fpack_map_set(FF_Fpack_Map *hashtable, int key,
                   FF_Font_Texture_Pack value) {
    unsigned int slot = int_hash(key) % HT_FPACK_TABLE_SIZE;

    struct FF_Fpack_Entry_Struct *entry = &hashtable->entries[slot];

    if (ENTRY_IS_EMPTY(*entry)) {
        hashtable->entries[slot] = fpack_map_entry_new(key, value);
        return;
    }

    struct FF_Fpack_Entry_Struct *prev = {0};
    struct FF_Fpack_Entry_Struct *head = entry;

    while (head) {
        if (head->key == key) {
            entry->value = value;
            return;
        }

        // walk to next
        prev = head;
        head = prev->next;
    }

    prev->next = (struct FF_Fpack_Entry_Struct *)calloc(
        1, sizeof(struct FF_Fpack_Entry_Struct));
    struct FF_Fpack_Entry_Struct next =
        fpack_map_entry_new(key, value);
    memcpy(prev->next, &next, sizeof(struct FF_Fpack_Entry_Struct));
}

FF_Font_Texture_Pack *fpack_map_get(FF_Fpack_Map *hashtable,
                                    int key) {
    unsigned int slot = int_hash(key) % HT_FPACK_TABLE_SIZE;

    struct FF_Fpack_Entry_Struct *entry = &hashtable->entries[slot];

    if (ENTRY_IS_EMPTY(*entry)) {
        return NULL;
    }

    FF_Fpack_Entry *head = entry;
    while (head != NULL) {
        if (head->key == key) {
            return &head->value;
        }

        head = head->next;
    }

    // reaching here means there were >= 1 entries but no key match
    return NULL;
}

FF_Fpack_Map fpack_map_create() {
    FF_Fpack_Map hashtable = {0};

    hashtable.entries =
        calloc(HT_FPACK_TABLE_SIZE, sizeof(FF_Fpack_Entry));

    return hashtable;
}

struct FF_Fpack_Entry_Struct fpack_map_entry_new(
    int key, const FF_Font_Texture_Pack value) {
    struct FF_Fpack_Entry_Struct result = {
        .not_empty = true,
        .key = key,
        .value = value,
        .next = 0,
    };
    return result;
}

void fpack_map_free(FF_Fpack_Map *ht) {
    for (ulong i = 0; i < HT_FPACK_TABLE_SIZE; ++i) {
        FF_Fpack_Entry entry = ht->entries[i];

        if (ENTRY_IS_EMPTY(entry)) continue;

        fpack_map_entry_free(entry);
    }
    free(ht->entries);
}

void fpack_map_entry_free(FF_Fpack_Entry entry) {
    // TODO FIX
    FF_Fpack_Entry *head = entry.next;
    while (head != NULL) {
        FF_Fpack_Entry *tmp = head;
        head = head->next;
        free(tmp);
    }
}
