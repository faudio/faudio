
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/map.h>
#include <doremir/set.h>
#include <doremir/string.h>
#include <doremir/util.h>

/*
    Notes:
        * We implement Map as a Set of entries
        * An entry is exactly like a pair, but compares on the first element only
        * Performance, memory etc depend entirely on Set implementation
 */

struct entry {
    impl_t      impl;       //  Interface dispatcher
    ptr_t       key;        //  Values
    ptr_t       value;
};

typedef struct entry * entry_t;

struct _doremir_map_t {
    impl_t          impl;       //  Interface dispatcher
    set_t           entries;    //  Set of entries
};


// --------------------------------------------------------------------------------

entry_t new_entry(doremir_ptr_t key, doremir_ptr_t value)
{
    doremir_ptr_t entry_impl(doremir_id_t interface);
    entry_t entry   = doremir_new_struct(entry);
    entry->impl     = &entry_impl;
    entry->key      = key;
    entry->value    = value;
    return entry;
}

entry_t copy_entry(entry_t entry)
{
    return new_entry(entry->key, entry->value);
}

void delete_entry(pair_t entry)
{
    doremir_delete(entry);
}

inline static map_t new_map(set_t entries)
{
    doremir_ptr_t map_impl(doremir_id_t interface);
    map_t map       = doremir_new(map);
    map->impl       = &map_impl;
    map->entries    = entries;
    return map;
}

inline static void delete_map(map_t map)
{
    doremir_delete(map);
}


// --------------------------------------------------------------------------------

doremir_map_t doremir_map_empty()
{
    return new_map(doremir_set_empty());
}

doremir_map_t doremir_map_add(doremir_map_key_t key, doremir_ptr_t value, doremir_map_t map)
{
    entry_t entry = new_entry(key, value);
    return new_map(doremir_set_add(entry, map->entries));
}

doremir_map_t doremir_map_set(doremir_map_key_t key, doremir_ptr_t value, doremir_map_t map)
{
    entry_t entry = new_entry(key, value);
    return new_map(doremir_set_set(entry, map->entries));
}

doremir_map_t doremir_map_remove(doremir_map_key_t key, doremir_map_t map)
{
    entry_t entry = new_entry(key, NULL); // we compare on keys, so value does not matter
    return new_map(doremir_set_dremove(entry, map->entries));
}

doremir_map_t doremir_map_dadd(doremir_map_key_t key, doremir_ptr_t value, doremir_map_t map)
{
    map_t map2 = doremir_map_add(key, value, map);
    doremir_map_destroy(map);
    return map2;
}

doremir_map_t doremir_map_dset(doremir_map_key_t key, doremir_ptr_t value, doremir_map_t map)
{
    map_t map2 = doremir_map_set(key, value, map);
    doremir_map_destroy(map);
    return map2;
}

doremir_map_t doremir_map_dremove(doremir_map_key_t key, doremir_map_t map)
{
    map_t map2 = doremir_map_remove(key, map);
    doremir_map_destroy(map);
    return map2;
}

doremir_map_t doremir_map_add_entry(doremir_pair_t x, doremir_map_t map)
{
    entry_t entry = new_entry(doremir_pair_fst(x), doremir_pair_snd(x));
    return new_map(doremir_set_add(entry, map->entries));
}

doremir_map_t doremir_map_remove_entry(doremir_pair_t x, doremir_map_t map)
{
    entry_t entry = new_entry(doremir_pair_fst(x), doremir_pair_snd(x));
    return new_map(doremir_set_remove(entry, map->entries));
}

doremir_map_t doremir_map_copy(doremir_map_t map)
{
    return new_map(doremir_set_copy(map->entries));
}

void doremir_map_destroy(doremir_map_t map)
{
    doremir_set_destroy(map->entries);
    doremir_delete(map);
}

doremir_ptr_t doremir_map_get(doremir_map_key_t key, doremir_map_t map)
{
    entry_t entry  = new_entry(key, NULL); // we compare on keys, so value does not matter
    entry_t entry2 = doremir_set_get(entry, map->entries);
    if (!entry2)
        return NULL;
    else
        return entry2->value;
}

bool doremir_map_has_key(doremir_map_key_t key, doremir_map_t map)
{
    entry_t entry  = new_entry(key, NULL); // we compare on keys, so value does not matter
    return doremir_set_get(entry, map->entries);
}

bool doremir_map_has_elem(doremir_ptr_t x, doremir_map_t map)
{
    // entry_t entry = new_entry(NULL, x);
    // return doremir_set_get(COMP_VALUE(entry), map->entries);
    assert(false && "Not implemented");
}

bool doremir_map_has_entry(doremir_pair_t entry, doremir_map_t map)
{
    // return doremir_set_get(COMP_BOTH(entry), map->entries);
    assert(false && "Not implemented");
}

int doremir_map_size(doremir_map_t map)
{
    return doremir_set_size(map->entries);
}

bool doremir_map_is_empty(doremir_map_t map)
{
    return doremir_set_is_empty(map->entries);
}

bool doremir_map_is_single(doremir_map_t map)
{
    return doremir_set_is_single(map->entries);
}

bool doremir_map_is_submap_of(doremir_map_t a, doremir_map_t b)
{
    return doremir_set_is_subset_of(a->entries, b->entries);
}

bool doremir_map_is_proper_submap_of(doremir_map_t a, doremir_map_t b)
{
    return doremir_set_is_proper_subset_of(a->entries, b->entries);
}

// --------------------------------------------------------------------------------

bool entry_equal(doremir_ptr_t a, doremir_ptr_t b)
{
    entry_t c = (entry_t) a;
    entry_t d = (entry_t) b;
    return doremir_equal(c->key, d->key);
}

bool entry_less_than(doremir_ptr_t a, doremir_ptr_t b)
{
    entry_t c = (entry_t) a;
    entry_t d = (entry_t) b;
    return doremir_less_than(c->key, d->key);
}

bool entry_greater_than(doremir_ptr_t a, doremir_ptr_t b)
{
    entry_t c = (entry_t) a;
    entry_t d = (entry_t) b;
    return doremir_greater_than(c->key, d->key);
}

doremir_string_t entry_show(doremir_ptr_t a)
{
    entry_t b = (entry_t) a;
    string_t s = string("<Entry (");
    s = string_dappend(s, doremir_string_show(b->key));
    s = string_dappend(s, string(","));       
    if (b->value)
        s = string_dappend(s, doremir_string_show(b->value));
    else
        s = string_dappend(s, string("null"));    
    s = string_dappend(s, string(")>"));
    return s;
}

doremir_ptr_t entry_copy(doremir_ptr_t a)
{
    return copy_entry(a);
}

void entry_destroy(doremir_ptr_t a)
{
    delete_entry(a);
}

doremir_ptr_t entry_impl(doremir_id_t interface)
{
    static doremir_equal_t entry_equal_impl = { entry_equal };
    static doremir_order_t entry_order_impl = { entry_less_than, entry_greater_than };
    static doremir_string_show_t entry_show_impl = { entry_show };
    static doremir_copy_t entry_copy_impl = { entry_copy };
    static doremir_destroy_t entry_destroy_impl = { entry_destroy };

    switch (interface) {
        case doremir_equal_i:
            return &entry_equal_impl;

        case doremir_order_i:
            return &entry_order_impl;

        case doremir_string_show_i:
            return &entry_show_impl;

        case doremir_copy_i:
            return &entry_copy_impl;

        case doremir_destroy_i:
            return &entry_destroy_impl;

        default:
            return NULL;
    }
}


// --------------------------------------------------------------------------------

bool map_equal(doremir_ptr_t a, doremir_ptr_t b)
{
    map_t c = (map_t) a;
    map_t d = (map_t) b;
    return doremir_equal(c->entries, d->entries);
}

bool map_less_than(doremir_ptr_t a, doremir_ptr_t b)
{
    map_t c = (map_t) a;
    map_t d = (map_t) b;
    return doremir_less_than(c->entries, d->entries);
}

bool map_greater_than(doremir_ptr_t a, doremir_ptr_t b)
{
    map_t c = (map_t) a;
    map_t d = (map_t) b;
    return doremir_greater_than(c->entries, d->entries);
}

doremir_string_t map_show(doremir_ptr_t x)
{
    map_t map = (map_t) x;
    string_t s  = string("{");

    doremir_for_each_last(x, doremir_set_to_list(map->entries), last) {
        entry_t entry = x;
        s = string_dappend(s, doremir_string_show(entry->key));
        s = string_dappend(s, string(": "));
        s = string_dappend(s, doremir_string_show(entry->value));

        if (!last) {
            s = string_dappend(s, string(", "));
        }
    }
    s = string_dappend(s, string("}"));
    return s;
}

doremir_ptr_t map_copy(doremir_ptr_t a)
{
    return doremir_map_copy(a);
}

void map_destroy(doremir_ptr_t a)
{
    doremir_map_destroy(a);
}

doremir_ptr_t map_impl(doremir_id_t interface)
{
    static doremir_equal_t map_equal_impl = { map_equal };
    static doremir_string_show_t map_show_impl = { map_show };
    static doremir_copy_t map_copy_impl = { map_copy };
    static doremir_destroy_t map_destroy_impl = { map_destroy };

    switch (interface) {
        case doremir_equal_i:
            return &map_equal_impl;

        case doremir_string_show_i:
            return &map_show_impl;

        case doremir_copy_i:
            return &map_copy_impl;

        case doremir_destroy_i:
            return &map_destroy_impl;

        default:
            return NULL;
    }
}

