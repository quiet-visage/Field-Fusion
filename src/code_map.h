#pragma once

typedef struct {
    int code_index;
    float advance[2];
} FF_Map_Item;

typedef struct FF_Code_Entry_Struct {
    int is_populated;
    int key;
    FF_Map_Item value;
    struct FF_Code_Entry_Struct *next;
} FF_Code_Entry;

typedef struct {
    FF_Map_Item ext_ascii[0xff];
    FF_Code_Entry *hash_table;
} FF_Map;

FF_Map ff_map_create(void);
void ff_map_destroy(FF_Map *m);
FF_Map_Item *ff_map_get(FF_Map *m, int code);
FF_Map_Item *ff_map_insert(FF_Map *m, int code);
