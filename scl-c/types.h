
#include <stdint.h>

// ------------------------------------------------------------------------------------------------

/** Unspecific pointer. */
typedef void* SclAny;

// ------------------------------------------------------------------------------------------------

/** Rational numbers */
typedef struct SclRatioHandle;
typedef SclRatioHandle* SclRatio;

typedef SclRatio SclRational;
typedef SclRatio SclRational32;
typedef SclRatio SclRational64;

SclRational  scl_rational_create(SclAny* nom, SclAny* denom, size_t size);
void         scl_rational_get_nom(SclRational pair, SclAny* value);
void         scl_rational_get_denom(SclRational pair, SclAny* value);
void         scl_rational__free(SclRational pair);


// ------------------------------------------------------------------------------------------------

/* Misc generic */

int scl_equal(SclAny, SclAny);
int scl_not_equal(SclAny, SclAny);
int scl_compare(SclAny, SclAny);

/** \return A sequence of scl_char. */
SclSequence scl_to_string(SclAny);

// ------------------------------------------------------------------------------------------------

/** 
    Pair structure 

    Note the unsafe create and get methods, each function that provides or accepts pairs is supposed 
    to document the polymorphic type.
  */
typedef struct SclPairHandle;
typedef SclPairHandle* SclPair;


SclPair scl_pair_create(SclAny* first, size_t firstSize, SclAny* second, size_t secondSize);
void    scl_pair_get_left(SclPair pair, SclAny* value);
void    scl_pair_get_right(SclPair pair, SclAny* value);
void    scl_pair_free(SclPair pair);


// ------------------------------------------------------------------------------------------------

/** 
    Sequence structure                             

    Note the unsafe create and get methods, each function that provides or accepts pairs is supposed 
    to document the polymorphic type.
  */
typedef struct SclSequenceHandle;
typedef SclSequenceHandle* SclSequence;

                
SclSequence     scl_sequence_empty();
SclSequence     scl_sequence_create(SclAny* value, size_t valueSize, SclSequence rest);

int             scl_sequence_is_empty(SclSequence value);
void            scl_sequence_get_first(SclSequence sequence, SclAny* value);
scl_sequence    scl_sequence_get_rest(SclSequence sequence);

void            scl_sequence_free(SclSequence sequence);
                


// ------------------------------------------------------------------------------------------------

typedef struct SclDynamicHandle;
typedef SclDynamicHandle* SclDynamic;

typedef enum SclType
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

SclType         scl_dynamic_type(SclDynamic value);

int             scl_dynamic_get_int(SclDynamic value);
int16_t         scl_dynamic_get_int16(SclDynamic value);
int32_t         scl_dynamic_get_int32(SclDynamic value);
int64_t         scl_dynamic_get_int64(SclDynamic value);
uint16_t        scl_dynamic_get_uint16(SclDynamic value);
uint32_t        scl_dynamic_get_uint32(SclDynamic value);
uint64_t        scl_dynamic_get_uint64(SclDynamic value);
char            scl_dynamic_get_char(SclDynamic value);
unsigned char   scl_dynamic_get_uchar(SclDynamic value);
signed char     scl_dynamic_get_schar(SclDynamic value);
float           scl_dynamic_get_float(SclDynamic value);
double          scl_dynamic_get_double(SclDynamic value);
SclPair         scl_dynamic_get_pair(SclDynamic value);
SclSequence     scl_dynamic_get_sequence(SclDynamic value);
                
SclDynamic      scl_dynamic_create_int(int value);
SclDynamic      scl_dynamic_create_uint(unsigned int value);
SclDynamic      scl_dynamic_create_int8(int8_t value);
SclDynamic      scl_dynamic_create_int16(int16_t value);
SclDynamic      scl_dynamic_create_int32(int32_t value);
SclDynamic      scl_dynamic_create_int64(int64_t value);
SclDynamic      scl_dynamic_create_uint8(uint8_t value);
SclDynamic      scl_dynamic_create_uint16(uint16_t value);
SclDynamic      scl_dynamic_create_uint32(uint32_t value);
SclDynamic      scl_dynamic_create_uint64(uint64_t value);
SclDynamic      scl_dynamic_create_char(char value);
SclDynamic      scl_dynamic_create_uchar(unsigned char value);
SclDynamic      scl_dynamic_create_schar(signed char value);
SclDynamic      scl_dynamic_create_float(float value);
SclDynamic      scl_dynamic_create_double(double value);
SclDynamic      scl_dynamic_create_pair(SclPair pair);
SclDynamic      scl_dynamic_create_sequence(SclSequence pair);

void            scl_dynamic_free(SclDynamic value);

