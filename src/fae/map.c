
/*
    FAE
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <fae/map.h>
#include <fae/set.h>
#include <fae/string.h>
#include <fae/dynamic.h>
#include <fae/util.h>

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

typedef struct entry *entry_t;

struct _fae_map_t {
    impl_t          impl;       //  Interface dispatcher
    set_t           entries;    //  Set of entries
};


// --------------------------------------------------------------------------------

entry_t new_entry(fae_ptr_t key, fae_ptr_t value)
{
    fae_ptr_t entry_impl(fae_id_t interface);
    entry_t entry   = fae_new_struct(entry);
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
    fae_delete(entry);
}

inline static map_t new_map(set_t entries)
{
    fae_ptr_t map_impl(fae_id_t interface);
    map_t map       = fae_new(map);
    map->impl       = &map_impl;
    map->entries    = entries;
    return map;
}

inline static void delete_map(map_t map)
{
    fae_delete(map->entries);
    fae_delete(map);
}


// --------------------------------------------------------------------------------

fae_map_t fae_map_empty()
{
    return new_map(fae_set_empty());
}

fae_map_t fae_map_add(fae_map_key_t key, fae_ptr_t value, fae_map_t map)
{
    entry_t entry = new_entry(key, value);
    return new_map(fae_set_add(entry, map->entries));
}

fae_map_t fae_map_set(fae_map_key_t key, fae_ptr_t value, fae_map_t map)
{
    entry_t entry = new_entry(key, value);
    return new_map(fae_set_set(entry, map->entries));
}

fae_map_t fae_map_remove(fae_map_key_t key, fae_map_t map)
{
    entry_t entry = new_entry(key, NULL); // we compare on keys, so value does not matter
    return new_map(fae_set_dremove(entry, map->entries));
}

fae_map_t fae_map_dadd(fae_map_key_t key, fae_ptr_t value, fae_map_t map)
{
    map_t map2 = fae_map_add(key, value, map);
    fae_map_destroy(map);
    return map2;
}

fae_map_t fae_map_dset(fae_map_key_t key, fae_ptr_t value, fae_map_t map)
{
    map_t map2 = fae_map_set(key, value, map);
    fae_map_destroy(map);
    return map2;
}

fae_map_t fae_map_dremove(fae_map_key_t key, fae_map_t map)
{
    map_t map2 = fae_map_remove(key, map);
    fae_map_destroy(map);
    return map2;
}

fae_map_t fae_map_add_entry(fae_pair_t x, fae_map_t map)
{
    entry_t entry = new_entry(fae_pair_fst(x), fae_pair_snd(x));
    return new_map(fae_set_add(entry, map->entries));
}

fae_map_t fae_map_remove_entry(fae_pair_t x, fae_map_t map)
{
    entry_t entry = new_entry(fae_pair_fst(x), fae_pair_snd(x));
    return new_map(fae_set_remove(entry, map->entries));
}

fae_map_t fae_map_copy(fae_map_t map)
{
    return new_map(fae_set_copy(map->entries));
}

void fae_map_destroy(fae_map_t map)
{
    fae_set_destroy(map->entries);
    fae_delete(map);
}

fae_ptr_t fae_map_get(fae_map_key_t key, fae_map_t map)
{
    entry_t entry  = new_entry(key, NULL); // we compare on keys, so value does not matter
    entry_t entry2 = fae_set_get(entry, map->entries);

    if (!entry2) {
        return NULL;
    } else {
        return entry2->value;
    }
}

bool fae_map_has_key(fae_map_key_t key, fae_map_t map)
{
    entry_t entry  = new_entry(key, NULL); // we compare on keys, so value does not matter
    return fae_set_get(entry, map->entries);
}

bool fae_map_has_elem(fae_ptr_t x, fae_map_t map)
{
    // entry_t entry = new_entry(NULL, x);
    // return fae_set_get(COMP_VALUE(entry), map->entries);
    assert(false && "Not implemented");
}

bool fae_map_has_entry(fae_pair_t entry, fae_map_t map)
{
    // return fae_set_get(COMP_BOTH(entry), map->entries);
    assert(false && "Not implemented");
}

int fae_map_size(fae_map_t map)
{
    return fae_set_size(map->entries);
}

bool fae_map_is_empty(fae_map_t map)
{
    return fae_set_is_empty(map->entries);
}

bool fae_map_is_single(fae_map_t map)
{
    return fae_set_is_single(map->entries);
}

bool fae_map_is_submap_of(fae_map_t a, fae_map_t b)
{
    return fae_set_is_subset_of(a->entries, b->entries);
}

bool fae_map_is_proper_submap_of(fae_map_t a, fae_map_t b)
{
    return fae_set_is_proper_subset_of(a->entries, b->entries);
}

fae_map_t fae_map_map(unary_t func, ptr_t data, fae_map_t map)
{
    map_t result = fae_map_empty();
    fae_map_for_each(key_val, map) {
        ptr_t key = fae_pair_fst(key_val);
        ptr_t val = fae_pair_snd(key_val);
        result = fae_map_add(key, func(data, val), result);
    }
    return result;
}



// --------------------------------------------------------------------------------

/** Create a set from the given elements.
 */
map_t fae_map(int count, ...)
{
    assert((count % 2 == 0)
           && "Map literal must have an even number of elements");

    map_t s = fae_map_empty();
    va_list args;

    va_start(args, count);

    for (int i = 0; i < count; i += 2) {
        ptr_t key = va_arg(args, ptr_t);
        ptr_t value = va_arg(args, ptr_t);
        s = fae_map_dadd(key, value, s);
    }

    va_end(args);
    return s;
}

pair_t entry_to_pair(ptr_t data, entry_t entry)
{
    return pair(entry->key, entry->value);
}
fae_list_t fae_map_to_list(fae_map_t map)
{
    return fae_list_map(
               (fae_unary_t) entry_to_pair, NULL,
               fae_set_to_list(map->entries));
}


// --------------------------------------------------------------------------------

bool entry_equal(fae_ptr_t a, fae_ptr_t b)
{
    entry_t c = (entry_t) a;
    entry_t d = (entry_t) b;
    return fae_equal(c->key, d->key);
}

bool entry_less_than(fae_ptr_t a, fae_ptr_t b)
{
    entry_t c = (entry_t) a;
    entry_t d = (entry_t) b;
    return fae_less_than(c->key, d->key);
}

bool entry_greater_than(fae_ptr_t a, fae_ptr_t b)
{
    entry_t c = (entry_t) a;
    entry_t d = (entry_t) b;
    return fae_greater_than(c->key, d->key);
}

fae_string_t entry_show(fae_ptr_t a)
{
    entry_t b = (entry_t) a;
    string_t s = string("<Entry (");
    s = string_dappend(s, fae_string_show(b->key));
    s = string_dappend(s, string(","));

    if (b->value) {
        s = string_dappend(s, fae_string_show(b->value));
    } else {
        s = string_dappend(s, string("null"));
    }

    s = string_dappend(s, string(")>"));
    return s;
}

fae_ptr_t entry_copy(fae_ptr_t a)
{
    return copy_entry(a);
}

void entry_destroy(fae_ptr_t a)
{
    delete_entry(a);
}

fae_ptr_t entry_impl(fae_id_t interface)
{
    static fae_equal_t entry_equal_impl = { entry_equal };
    static fae_order_t entry_order_impl = { entry_less_than, entry_greater_than };
    static fae_string_show_t entry_show_impl = { entry_show };
    static fae_copy_t entry_copy_impl = { entry_copy };
    static fae_destroy_t entry_destroy_impl = { entry_destroy };

    switch (interface) {
    case fae_equal_i:
        return &entry_equal_impl;

    case fae_order_i:
        return &entry_order_impl;

    case fae_string_show_i:
        return &entry_show_impl;

    case fae_copy_i:
        return &entry_copy_impl;

    case fae_destroy_i:
        return &entry_destroy_impl;

    default:
        return NULL;
    }
}


// --------------------------------------------------------------------------------

bool map_equal(fae_ptr_t a, fae_ptr_t b)
{
    map_t c = (map_t) a;
    map_t d = (map_t) b;
    return fae_equal(c->entries, d->entries);
}

bool map_less_than(fae_ptr_t a, fae_ptr_t b)
{
    map_t c = (map_t) a;
    map_t d = (map_t) b;
    return fae_less_than(c->entries, d->entries);
}

bool map_greater_than(fae_ptr_t a, fae_ptr_t b)
{
    map_t c = (map_t) a;
    map_t d = (map_t) b;
    return fae_greater_than(c->entries, d->entries);
}

fae_string_t map_show(fae_ptr_t x)
{
    map_t map = (map_t) x;
    string_t s  = string("{");

    fae_for_each_last(x, fae_set_to_list(map->entries), last) {
        entry_t entry = x;
        s = string_dappend(s, fae_string_show(entry->key));
        s = string_dappend(s, string(": "));
        s = string_dappend(s, fae_string_show(entry->value));

        if (!last) {
            s = string_dappend(s, string(", "));
        }
    }
    s = string_dappend(s, string("}"));
    return s;
}

fae_ptr_t map_copy(fae_ptr_t a)
{
    return fae_map_copy(a);
}

void map_destroy(fae_ptr_t a)
{
    fae_map_destroy(a);
}

type_repr_t map_get_type(fae_ptr_t a)
{
    return map_type_repr;
}

fae_ptr_t map_impl(fae_id_t interface)
{
    static fae_equal_t map_equal_impl = { map_equal };
    static fae_string_show_t map_show_impl = { map_show };
    static fae_copy_t map_copy_impl = { map_copy };
    static fae_destroy_t map_destroy_impl = { map_destroy };
    static fae_dynamic_t map_dynamic_impl = { map_get_type };

    switch (interface) {
    case fae_equal_i:
        return &map_equal_impl;

    case fae_string_show_i:
        return &map_show_impl;

    case fae_copy_i:
        return &map_copy_impl;

    case fae_destroy_i:
        return &map_destroy_impl;

    case fae_dynamic_i:
        return &map_dynamic_impl;

    default:
        return NULL;
    }
}

