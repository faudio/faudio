
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.

 */

#include <fa/string.h>
#include <fa/dynamic.h>
#include <fa/util.h>
#include <fa/map.h>
#include <fa/list.h>

/*
    ## Notes


 */

struct entry {
    struct entry    *next;      //  Next entry or null
    fa_ptr_t        key;        //  The key
    fa_ptr_t        value;      //  The value
};

typedef struct entry *entry_t;

struct _fa_map_t {
    fa_impl_t           impl;       //  Interface dispatcher
    entry_t             entry;      //  Top-level entry
    fa_map_destructor_t value_dest; //  Value destructor
};

static int gEntryCount = 0;
static int gMapCount   = 0;


// --------------------------------------------------------------------------------

inline static entry_t new_entry(fa_ptr_t key, fa_ptr_t value, entry_t next)
{
    entry_t entry = fa_new_struct(entry);
    entry->key   = key;
    entry->value = value;
    entry->next  = next;
    gEntryCount++;
    return entry;
}


inline static void delete_entry(entry_t entry)
{
    gEntryCount--;
    fa_delete(entry);
}
    
inline static void destroy_entries(entry_t entry, fa_map_destructor_t value_destructor)
{
    if (!entry) {
        return;
    }

    destroy_entries(entry->next, value_destructor);
    if (entry->key) fa_destroy(entry->key);
    if (entry->value && value_destructor) {
        value_destructor(entry->value);
    }
    delete_entry(entry);
}

fa_ptr_t map_impl(fa_id_t interface);

inline static fa_map_t new_map(entry_t entry, fa_map_destructor_t value_destructor)
{
    fa_map_t map = fa_new(map);
    map->impl = &map_impl;
    map->entry = entry;
    map->value_dest = value_destructor;
    gMapCount++;
    return map;
}

inline static void delete_map(fa_map_t map)
{
    gMapCount--;
    fa_delete(map);
}

/** Iterate over the entrys of a map. The variable var will be a entry_t referencing
    the entry in the following block.

    impl_for_each_entry(my_map, entry)
        fa_print("%s\n", entry->value);
 */
#define impl_for_each_entry(map, var) \
    for(entry_t _n = map->entry; _n; _n = _n->next) \
        fa_let(var, _n)

/** Iterate over the keys of a map. The variable var will be a fa_ptr_t
    referencing the value in the following block.

    This macro is independent from the foreach in <fa/utils.h>, which should
    not be used in this file.

    impl_for_each_key(my_map, key)
        fa_print("%s\n", key);

 */
#define impl_for_each_key(map, var) \
    for(entry_t _n = map->entry; _n; _n = _n->next) \
        fa_let(var, _n->key)

/** The begin_entry, append_entry and prepend_entry macros can be used to construct
    a map in place.

    For example:

        begin_entry(entry, next);
        while (...)
          if (...)
            append_entry(next, value);
          else
            prepend_entry(next, value);
        return new_map(entry);

    @param place
        A entry_t pointer.
    @param value
        Value to add.
 */

#define begin_entry(var, next) \
    entry_t var = NULL, *next = &var

#define append_entry(place, key, value) \
    do {                                        \
        *place = new_entry(key, value, NULL);         \
        place = &(*place)->next;                \
    } while (0)

#define prepend_entry(place, key, value) \
    do {                                        \
        *place = new_entry(key, value, *place);       \
    } while (0)


// --------------------------------------------------------------------------------

fa_map_t fa_map_empty()
{
    return new_map(NULL, NULL);
}

fa_map_t fa_map_single(fa_ptr_t key, fa_ptr_t value)
{
    return new_map(new_entry(key, value, NULL), NULL);
}

fa_map_t fa_map_copy(fa_map_t xs)
{
    // copy head element
    entry_t old = xs->entry;
    entry_t new_head = new_entry(fa_copy(old->key), old->value, NULL);

    // copy rest of the map
    entry_t new = new_head;
    old = old->next;
    while (old != NULL) {
        new->next = new_entry(fa_copy(old->key), old->value, NULL);
        old = old->next;
        new = new->next;
    }

    return new_map(new_head, xs->value_dest);
}

static inline fa_ptr_t deep_copy_unless_null(fa_ptr_t x)
{
    return x ? fa_deep_copy(x) : NULL;
}

fa_map_t fa_map_deep_copy(fa_map_t xs)
{
    // copy head element
    entry_t old = xs->entry;
    entry_t new_head = new_entry(deep_copy_unless_null(old->key), deep_copy_unless_null(old->value), NULL);

    // copy rest of the map
    entry_t new = new_head;
    old = old->next;
    while (old != NULL) {
        new->next = new_entry(deep_copy_unless_null(old->key), deep_copy_unless_null(old->value), NULL);
        old = old->next;
        new = new->next;
    }

    return new_map(new_head, xs->value_dest);
}

void fa_map_destroy(fa_map_t xs)
{
    destroy_entries(xs->entry, xs->value_dest);
    delete_map(xs);
}

void fa_map_deep_destroy(fa_map_t xs, fa_deep_destroy_pred_t p)
{
    if (!p(xs)) return;
    impl_for_each_entry(xs, entry) {
        fa_deep_destroy(entry->key, p);
        fa_deep_destroy(entry->value, p);
        entry->key = NULL;
        entry->value = NULL;
    }
    destroy_entries(xs->entry, xs->value_dest);
    delete_map(xs);
}

void fa_map_set_value_destructor(fa_map_t map, fa_map_destructor_t fn)
{
    map->value_dest = fn;
}

bool fa_map_is_empty(fa_map_t xs)
{
    return xs->entry == NULL;
}

bool fa_map_is_single(fa_map_t xs)
{
    return xs->entry && !xs->entry->next;
}

int fa_map_size(fa_map_t xs)
{
    int count = 0;
    impl_for_each_entry(xs, entry) {
        entry = 0; // kill warning
        count++;
    }
    return count;
}




// --------------------------------------------------------------------------------


fa_ptr_t fa_map_get(fa_ptr_t key, fa_map_t map)
{
    impl_for_each_entry(map, entry) {
        if (fa_equal(key, entry->key)) {
            return entry->value;
        }
    }
    return NULL;
}

fa_ptr_t fa_map_dget(fa_ptr_t key, fa_map_t map)
{
    fa_ptr_t value = fa_map_get(key, map);
    fa_destroy(key);
    return value;
}

fa_map_t fa_map_dset(fa_ptr_t key, fa_ptr_t value, fa_map_t map)
{
    impl_for_each_entry(map, entry) {
        if (fa_equal(key, entry->key)) {
            fa_map_destructor_t destructor = map->value_dest;
            if (destructor && entry->value) {
                destructor(entry->value);
            }
            entry->value = value;
            fa_destroy(key);
            return map;
        }
    }
    // No match, insert new entry
    fa_map_t map2 = new_map(new_entry(key, value, map->entry), map->value_dest);
    delete_map(map);
    return map2;
}

fa_map_t fa_map_set(fa_ptr_t key, fa_ptr_t value, fa_map_t map)
{
    return fa_map_dset(fa_copy(key), fa_copy(value), fa_copy(map));
}

fa_map_t fa_map_dadd(fa_ptr_t key, fa_ptr_t value, fa_map_t map)
{
    fa_map_destructor_t destructor = map->value_dest;
    impl_for_each_entry(map, entry) {
        if (fa_equal(key, entry->key)) {
            fa_destroy(key);
            if (destructor) {
                destructor(value);
            }
            return map;
        }
    }
    fa_map_t map2 = new_map(new_entry(key, value, map->entry), map->value_dest);
    delete_map(map);
    return map2;
}

fa_map_t fa_map_dremove(fa_ptr_t key, fa_map_t map)
{
    fa_map_destructor_t destructor = map->value_dest;
    entry_t last_entry = NULL;
    impl_for_each_entry(map, entry) {
        if (fa_equal(key, entry->key)) {
            if (destructor) {
                destructor(entry->value);
            }
            if (last_entry) {
                last_entry->next = entry->next;
            } else {
                map->entry = entry->next;
            }
            delete_entry(entry);
            fa_destroy(key);
            return map;
        }
        last_entry = entry;
    }
    fa_destroy(key);
    return map;
}

fa_map_t fa_map_remove(fa_ptr_t key, fa_map_t map)
{
    return fa_map_dremove(fa_copy(key), fa_copy(map));
}


// --------------------------------------------------------------------------------

fa_map_t fa_map(int count, ...)
{
    assert((count % 2 == 0) && "Map literal must have an even number of elements");
    
    va_list args;
    va_start(args, count);
    begin_entry(entry, next);

    for (int i = 0; i < count; i += 2) {
        append_entry(next, va_arg(args, fa_ptr_t), va_arg(args, fa_ptr_t));
    }

    va_end(args);
    return new_map(entry, NULL);
}

fa_map_t fa_map_from_list(fa_list_t list)
{
    begin_entry(entry, next);

    fa_for_each(pair, list) {
        append_entry(next, fa_copy(fa_pair_first(pair)), fa_copy(fa_pair_second(pair)));
    }

    return new_map(entry, NULL);
}

fa_map_t fa_map_dfrom_list(fa_list_t list)
{
    begin_entry(entry, next);

    fa_for_each(pair, list) {
        append_entry(next, fa_pair_first(pair), fa_pair_second(pair));
        fa_destroy(pair);
    }
    fa_destroy(list);

    return new_map(entry, NULL);
}



// fa_map_t fa_map_from_list(fa_list_t list)
// {
//     begin_entry(entry, next);
//
//     int count = 0;
//     fa_ptr_t key;
//     fa_for_each(x, list) {
//         fa_slog_info("  in foreach, x: ", x);
//         if (count % 2 == 0) {
//             fa_slog_info("    setting key");
//             key = x;
//         } else {
//             fa_slog_info("    appending entry");
//             append_entry(next, key, x);
//         }
//         count++;
//     }
//
//     fa_map_t map = new_map(entry);
//     fa_slog_info("fa_map_from_list: ", list, map);
//
//     return map;
// }

fa_list_t fa_map_to_list(fa_map_t map)
{
    assert(false && "Not implemented");
    return NULL;
}

fa_list_t fa_map_get_keys(fa_map_t map)
{
    fa_list_t keys = fa_list_empty();
    impl_for_each_entry(map, entry) {
        fa_push_list(entry->key, keys);
    }
    return keys;
}


// --------------------------------------------------------------------------------

bool map_equal(fa_ptr_t map1, fa_ptr_t map2)
{
    if (fa_map_size(map1) != fa_map_size(map2)) return false;
    
    fa_map_t m1 = map1;
    
    impl_for_each_entry(m1, entry) {
        if (!fa_equal(entry->value, fa_map_get(entry->key, map2))) return false;
    }
    return true;
}

fa_string_t map_show(fa_ptr_t map)
{
    fa_string_t result  = fa_string("");
    entry_t   entry = ((fa_map_t) map)->entry;
    result = fa_string_dappend(result, fa_string("{"));

    while (entry) {
        result  = fa_string_dappend(result, fa_string_show(entry->key));
        result  = fa_string_dappend(result, fa_string(": "));
        result  = fa_string_dappend(result, fa_string_show(entry->value));
        entry   = entry->next;

        if (entry) {
            result = fa_string_dappend(result, fa_string(", "));
        }
    };

    result = fa_string_dappend(result, fa_string("}"));

    return result;
}

fa_ptr_t map_copy(fa_ptr_t a)
{
    return fa_map_copy(a);
}

fa_ptr_t map_deep_copy(fa_ptr_t a)
{
  return fa_map_deep_copy(a);
}

void map_destroy(fa_ptr_t a)
{
    fa_map_destroy(a);
}

void map_deep_destroy(fa_ptr_t a, fa_deep_destroy_pred_t p)
{
    fa_map_deep_destroy(a, p);
}

fa_dynamic_type_repr_t map_get_type(fa_ptr_t a)
{
    return map_type_repr;
}

fa_ptr_t map_impl(fa_id_t interface)
{
    static fa_equal_t map_equal_impl
        = { map_equal };
    static fa_string_show_t map_show_impl
        = { map_show };
    static fa_copy_t map_copy_impl
        = { map_copy, map_deep_copy };
    static fa_destroy_t map_destroy_impl
        = { map_destroy, map_deep_destroy };
    static fa_dynamic_t map_dynamic_impl
        = { map_get_type };

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

void fa_map_log_count()
{
  fa_log_info(fa_string_dappend(fa_string("Maps/entrys allocated: "),
      fa_string_dappend(fa_string_dshow(fa_i32(gMapCount)),
      fa_string_dappend(fa_string(" / "), fa_string_dshow(fa_i32(gEntryCount))))));
}

