
#ifndef _DOREMIR_TYPE
#define _DOREMIR_TYPE

#include <doremir.h>
#include <doremir/pair.h>

/** @defgroup Doremir Doremir
    @{
    @defgroup DoremirType Type
    @{
    */

typedef size_t doremir_type_frames_t;
typedef enum {
            uint8_type, double_type
        } doremir_type_simple_t;
typedef struct {
            doremir_impl_t impl;
            enum {
                simple_type, pair_type, vector_type, frame_type
            } tag;
            union {
                doremir_type_simple_t simple;
                struct {
                    doremir_ptr_t fst; doremir_ptr_t snd;
                } pair;
                struct {
                    doremir_ptr_t base; size_t size;
                } vector;
                struct {
                    doremir_ptr_t base;
                } frame;
            } fields;
        } doremir_type_struct_t;
typedef doremir_type_struct_t * doremir_type_t;
doremir_type_t doremir_type_simple(doremir_type_simple_t);
doremir_type_t doremir_type_pair(doremir_type_t, doremir_type_t);
doremir_type_t doremir_type_vector(doremir_type_t, size_t);
doremir_type_t doremir_type_frame(doremir_type_t);
doremir_type_t doremir_type_copy(doremir_type_t);
void doremir_type_destroy(doremir_type_t);
bool doremir_type_is_simple(doremir_type_t);
bool doremir_type_is_pair(doremir_type_t);
bool doremir_type_is_vector(doremir_type_t);
bool doremir_type_is_frame(doremir_type_t);
size_t doremir_type_size_of(doremir_type_frames_t, doremir_type_t);
size_t doremir_type_align_of(doremir_type_t);

/** @}
    @}
    */

#endif // _DOREMIR_TYPE

