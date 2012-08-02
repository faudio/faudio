
#pragma once
#include <stdint.h>

// ------------------------------------------------------------------------------------------------

/** Unspecific pointer. */
typedef void* scl_any;

// ------------------------------------------------------------------------------------------------

/* Misc generic */

int scl_equal(scl_any, scl_any);
int scl_not_equal(scl_any, scl_any);
int scl_compare(scl_any, scl_any);

/** \return A sequence of scl_char. */
scl_sequence scl_to_string(scl_any);

// ------------------------------------------------------------------------------------------------

/** Pair structure 

    Note the unsafe create and get methods, each function that provides or accepts pairs is supposed 
    to document the polymorphic type.
  */
typedef struct scl_pair_handle;
typedef scl_pair_handle* scl_pair;


scl_pair        scl_pair_create(scl_any* value, scl_any* value);
void            scl_pair_get_left(scl_pair pair, scl_any* value);
void            scl_pair_get_right(scl_pair pair, scl_any* value);
void            scl_pair_free(scl_pair pair);


// ------------------------------------------------------------------------------------------------

/** Sequence structure                             

    Note the unsafe create and get methods, each function that provides or accepts pairs is supposed 
    to document the polymorphic type.
  */
typedef struct scl_sequence_handle;
typedef scl_sequence_handle* scl_sequence;

                
scl_sequence    scl_sequence_empty();
scl_sequence    scl_sequence_create(scl_any* value, scl_sequence rest);

int             scl_sequence_is_empty(scl_sequence value);
void            scl_sequence_get_first(scl_sequence sequence, scl_any* value);
scl_sequence    scl_sequence_get_rest(scl_sequence sequence);

void            scl_sequence_free(scl_sequence sequence);
                


// ------------------------------------------------------------------------------------------------

/** A full-fledged discriminated union.
 */
typedef struct scl_dynamic_handle;
typedef scl_dynamic_handle* scl_dynamic;

typedef enum scl_type
{
  scl_type_int;
  scl_type_uint;
  scl_type_int8;
  scl_type_int16;
  scl_type_int32;
  scl_type_uint8;
  scl_type_uint16;
  scl_type_uint32;
  scl_type_char;
  scl_type_uchar;
  scl_type_schar;
  scl_type_float;
  scl_type_double;
  scl_type_pair;
  scl_type_sequence;
};

scl_type        scl_dynamic_type(scl_dynamic value);

int             scl_dynamic_get_int(scl_dynamic value);
int16_t         scl_dynamic_get_int16_t(scl_dynamic value);
int32_t         scl_dynamic_get_int32_t(scl_dynamic value);
int64_t         scl_dynamic_get_int64_t(scl_dynamic value);
uint16_t        scl_dynamic_get_uint16_t(scl_dynamic value);
uint32_t        scl_dynamic_get_uint32_t(scl_dynamic value);
uint64_t        scl_dynamic_get_uint64_t(scl_dynamic value);
char            scl_dynamic_get_char(scl_dynamic value);
unsigned char   scl_dynamic_get_uchar(scl_dynamic value);
signed char     scl_dynamic_get_schar(scl_dynamic value);
float           scl_dynamic_get_float(scl_dynamic value);
double          scl_dynamic_get_double(scl_dynamic value);
scl_pair        scl_dynamic_get_pair(scl_dynamic value);
scl_sequence    scl_dynamic_get_sequence(sequence_t value);

scl_dynamic     scl_dynamic_create_int(int value);
scl_dynamic     scl_dynamic_create_uint(unsigned int value);
scl_dynamic     scl_dynamic_create_int8_t(int8_t value);
scl_dynamic     scl_dynamic_create_int16_t(int16_t value);
scl_dynamic     scl_dynamic_create_int32_t(int32_t value);
scl_dynamic     scl_dynamic_create_int64_t(int64_t value);
scl_dynamic     scl_dynamic_create_uint8_t(uint8_t value);
scl_dynamic     scl_dynamic_create_uint16_t(uint16_t value);
scl_dynamic     scl_dynamic_create_uint32_t(uint32_t value);
scl_dynamic     scl_dynamic_create_uint64_t(uint64_t value);
scl_dynamic     scl_dynamic_create_char(char value);
scl_dynamic     scl_dynamic_create_uchar(unsigned char value);
scl_dynamic     scl_dynamic_create_schar(signed char value);
scl_dynamic     scl_dynamic_create_float(float value);
scl_dynamic     scl_dynamic_create_double(double value);
scl_dynamic     scl_dynamic_create_pair(pair_t pair);
scl_dynamic     scl_dynamic_create_sequence(sequence_t pair);

void            scl_dynamic_free(scl_dynamic value);

