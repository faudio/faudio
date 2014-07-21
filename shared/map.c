
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.

 */

#include <fa/map.h>
#include <fa/set.h>
#include <fa/string.h>
#include <fa/dynamic.h>
#include <fa/util.h>

/*
    ## Notes

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

struct _fa_map_t {
    impl_t          impl;       //  Interface dispatcher
    set_t           entries;    //  Set of entries
};


// --------------------------------------------------------------------------------

entry_t new_entry(fa_ptr_t key, fa_ptr_t value)
{
    fa_ptr_t entry_impl(fa_id_t interface);
    entry_t entry   = fa_new_struct(entry);
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
    fa_delete(entry);
}

inline static map_t new_map(set_t entries)
{
    fa_ptr_t map_impl(fa_id_t interface);
    map_t map       = fa_new(map);
    map->impl       = &map_impl;
    map->entries    = entries;
    return map;
}

inline static void delete_map(map_t map)
{
    fa_delete(map->entries);
    fa_delete(map);
}


// --------------------------------------------------------------------------------

fa_map_t fa_map_empty()
{
    return new_map(fa_set_empty());
}

fa_map_t fa_map_add(fa_map_key_t key, fa_ptr_t value, fa_map_t map)
{
    entry_t entry = new_entry(key, value);
    return new_map(fa_set_add(entry, map->entries));
}

fa_map_t fa_map_set(fa_map_key_t key, fa_ptr_t value, fa_map_t map)
{
    entry_t entry = new_entry(key, value);
    return new_map(fa_set_set(entry, map->entries));
}

fa_map_t fa_map_remove(fa_map_key_t key, fa_map_t map)
{
    entry_t entry = new_entry(key, NULL); // we compare on keys, so value does not matter
    return new_map(fa_set_dremove(entry, map->entries));
}

fa_map_t fa_map_dadd(fa_map_key_t key, fa_ptr_t value, fa_map_t map)
{
    map_t map2 = fa_map_add(key, value, map);
    fa_map_destroy(map);
    return map2;
}

fa_map_t fa_map_dset(fa_map_key_t key, fa_ptr_t value, fa_map_t map)
{
    map_t map2 = fa_map_set(key, value, map);
    fa_map_destroy(map);
    return map2;
}

fa_map_t fa_map_dremove(fa_map_key_t key, fa_map_t map)
{
    map_t map2 = fa_map_remove(key, map);
    fa_map_destroy(map);
    return map2;
}

fa_map_t fa_map_add_entry(fa_pair_t x, fa_map_t map)
{
    entry_t entry = new_entry(fa_pair_first(x), fa_pair_second(x));
    return new_map(fa_set_add(entry, map->entries));
}

fa_map_t fa_map_remove_entry(fa_pair_t x, fa_map_t map)
{
    entry_t entry = new_entry(fa_pair_first(x), fa_pair_second(x));
    return new_map(fa_set_remove(entry, map->entries));
}

fa_map_t fa_map_copy(fa_map_t map)
{
    return new_map(fa_set_copy(map->entries));
}

void fa_map_destroy(fa_map_t map)
{
    fa_set_destroy(map->entries);
    fa_delete(map);
}

fa_ptr_t fa_map_get(fa_map_key_t key, fa_map_t map)
{
    entry_t entry  = new_entry(key, NULL); // we compare on keys, so value does not matter
    entry_t entry2 = fa_set_get(entry, map->entries);

    if (!entry2) {
        return NULL;
    } else {
        return entry2->value;
    }
}

bool fa_map_has_key(fa_map_key_t key, fa_map_t map)
{
    entry_t entry  = new_entry(key, NULL); // we compare on keys, so value does not matter
    return fa_set_get(entry, map->entries);
}

bool fa_map_has_elem(fa_ptr_t x, fa_map_t map)
{
    // entry_t entry = new_entry(NULL, x);
    // return fa_set_get(COMP_VALUE(entry), map->entries);
    assert(false && "Not implemented");
}

bool fa_map_has_entry(fa_pair_t entry, fa_map_t map)
{
    // return fa_set_get(COMP_BOTH(entry), map->entries);
    assert(false && "Not implemented");
}

int fa_map_size(fa_map_t map)
{
    return fa_set_size(map->entries);
}

bool fa_map_is_empty(fa_map_t map)
{
    return fa_set_is_empty(map->entries);
}

bool fa_map_is_single(fa_map_t map)
{
    return fa_set_is_single(map->entries);
}

bool fa_map_is_submap_of(fa_map_t a, fa_map_t b)
{
    return fa_set_is_subset_of(a->entries, b->entries);
}

bool fa_map_is_proper_submap_of(fa_map_t a, fa_map_t b)
{
    return fa_set_is_proper_subset_of(a->entries, b->entries);
}

fa_map_t fa_map_map(unary_t func, ptr_t data, fa_map_t map)
{
    map_t result = fa_map_empty();
    fa_map_for_each(key_val, map) {
        ptr_t key = fa_pair_first(key_val);
        ptr_t val = fa_pair_second(key_val);
        result = fa_map_add(key, func(data, val), result);
    }
    return result;
}

fa_map_t fa_map_sum(fa_map_t a, fa_map_t b)
{
    return new_map(fa_set_sum(a->entries, b->entries));
}

fa_map_t fa_map_intersection(fa_map_t a, fa_map_t b)
{
    return new_map(fa_set_sum(a->entries, b->entries));
}

fa_map_t fa_map_difference(fa_map_t a, fa_map_t b)
{
    return new_map(fa_set_sum(a->entries, b->entries));
}



// --------------------------------------------------------------------------------

/** Create a set from the given elements.
 */
map_t fa_map(int count, ...)
{
    assert((count % 2 == 0)
           && "Map literal must have an even number of elements");

    map_t s = fa_map_empty();
    va_list args;

    va_start(args, count);

    for (int i = 0; i < count; i += 2) {
        ptr_t key = va_arg(args, ptr_t);
        ptr_t value = va_arg(args, ptr_t);
        s = fa_map_dadd(key, value, s);
    }

    va_end(args);
    return s;
}

pair_t entry_to_pair(ptr_t data, entry_t entry)
{
    return fa_pair_create(entry->key, entry->value);
}
fa_list_t fa_map_to_list(fa_map_t map)
{
    return fa_list_map(
               (fa_unary_t) entry_to_pair, NULL,
               fa_set_to_list(map->entries));
}


// --------------------------------------------------------------------------------

bool entry_equal(fa_ptr_t a, fa_ptr_t b)
{
    entry_t c = (entry_t) a;
    entry_t d = (entry_t) b;
    return fa_equal(c->key, d->key);
}

bool entry_less_than(fa_ptr_t a, fa_ptr_t b)
{
    entry_t c = (entry_t) a;
    entry_t d = (entry_t) b;
    return fa_less_than(c->key, d->key);
}

bool entry_greater_than(fa_ptr_t a, fa_ptr_t b)
{
    entry_t c = (entry_t) a;
    entry_t d = (entry_t) b;
    return fa_greater_than(c->key, d->key);
}

fa_string_t entry_show(fa_ptr_t a)
{
    entry_t b = (entry_t) a;
    string_t s = string("<Entry (");
    s = string_dappend(s, fa_string_show(b->key));
    s = string_dappend(s, string(","));

    if (b->value) {
        s = string_dappend(s, fa_string_show(b->value));
    } else {
        s = string_dappend(s, string("null"));
    }

    s = string_dappend(s, string(")>"));
    return s;
}

fa_ptr_t entry_copy(fa_ptr_t a)
{
    return copy_entry(a);
}

void entry_destroy(fa_ptr_t a)
{
    delete_entry(a);
}

fa_ptr_t entry_impl(fa_id_t interface)
{
    static fa_equal_t entry_equal_impl = { entry_equal };
    static fa_order_t entry_order_impl = { entry_less_than, entry_greater_than };
    static fa_string_show_t entry_show_impl = { entry_show };
    static fa_copy_t entry_copy_impl = { entry_copy };
    static fa_destroy_t entry_destroy_impl = { entry_destroy };

    switch (interface) {
    case fa_equal_i:
        return &entry_equal_impl;

    case fa_order_i:
        return &entry_order_impl;

    case fa_string_show_i:
        return &entry_show_impl;

    case fa_copy_i:
        return &entry_copy_impl;

    case fa_destroy_i:
        return &entry_destroy_impl;

    default:
        return NULL;
    }
}


// --------------------------------------------------------------------------------

bool map_equal(fa_ptr_t a, fa_ptr_t b)
{
    map_t c = (map_t) a;
    map_t d = (map_t) b;
    return fa_equal(c->entries, d->entries);
}

bool map_less_than(fa_ptr_t a, fa_ptr_t b)
{
    map_t c = (map_t) a;
    map_t d = (map_t) b;
    return fa_less_than(c->entries, d->entries);
}

bool map_greater_than(fa_ptr_t a, fa_ptr_t b)
{
    map_t c = (map_t) a;
    map_t d = (map_t) b;
    return fa_greater_than(c->entries, d->entries);
}

fa_string_t map_show(fa_ptr_t x)
{
    map_t map = (map_t) x;
    string_t s  = string("{");

    fa_for_each_last(x, fa_set_to_list(map->entries), last) {
        entry_t entry = x;
        s = string_dappend(s, fa_string_show(entry->key));
        s = string_dappend(s, string(": "));
        s = string_dappend(s, fa_string_show(entry->value));

        if (!last) {
            s = string_dappend(s, string(", "));
        }
    }
    s = string_dappend(s, string("}"));
    return s;
}

fa_ptr_t map_copy(fa_ptr_t a)
{
    return fa_map_copy(a);
}

void map_destroy(fa_ptr_t a)
{
    fa_map_destroy(a);
}

type_repr_t map_get_type(fa_ptr_t a)
{
    return map_type_repr;
}

fa_ptr_t map_impl(fa_id_t interface)
{
    static fa_equal_t map_equal_impl = { map_equal };
    static fa_string_show_t map_show_impl = { map_show };
    static fa_copy_t map_copy_impl = { map_copy };
    static fa_destroy_t map_destroy_impl = { map_destroy };
    static fa_dynamic_t map_dynamic_impl = { map_get_type };

    switch (interface) {
    case fa_equal_i:
        return &map_equal_impl;

    case fa_string_show_i:
        return &map_show_impl;

    case fa_copy_i:
        return &map_copy_impl;

    case fa_destroy_i:
        return &map_destroy_impl;

    case fa_dynamic_i:
        return &map_dynamic_impl;

    default:
        return NULL;
    }
}

