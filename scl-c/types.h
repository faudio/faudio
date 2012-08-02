
#include <stdint.h>

typedef enum SclType
{
  SclTypeInt;
  SclTypeUInt;
  SclTypeInt8;
  SclTypeInt16;
  SclTypeInt32;
  SclTypeUInt8;
  SclTypeUInt16;
  SclTypeUInt32;
  SclTypeChar;
  SclTypeUChar;
  SclTypeSChar;
  SclTypeFloat;
  SclTypeDouble;
  SclTypePair;
  SclTypeSequence;
};

typedef struct SclDynamicOpaque;
typedef SclDynamicOpaque* SclDynamic;


SclType       scl_dynamic_type(SclDynamic value);

int           scl_dynamic_get_int(SclDynamic value);
unsigned int  scl_dynamic_get_uint(SclDynamic value);
int8_t        scl_dynamic_get_int8_t(SclDynamic value);
int16_t       scl_dynamic_get_int16_t(SclDynamic value);
int32_t       scl_dynamic_get_int32_t(SclDynamic value);
int64_t       scl_dynamic_get_int64_t(SclDynamic value);
uint8_t       scl_dynamic_get_uint8_t(SclDynamic value);
uint16_t      scl_dynamic_get_uint16_t(SclDynamic value);
uint32_t      scl_dynamic_get_uint32_t(SclDynamic value);
uint64_t      scl_dynamic_get_uint64_t(SclDynamic value);
char          scl_dynamic_get_char(SclDynamic value);
unsigned char scl_dynamic_get_uchar(SclDynamic value);
signed char   scl_dynamic_get_schar(SclDynamic value);
float         scl_dynamic_get_float(SclDynamic value);
double        scl_dynamic_get_double(SclDynamic value);
SclPair       scl_dynamic_get_pair(SclDynamic value);
SclSequence   scl_dynamic_get_pair(sequence_t value);

SclDynamic     scl_dynamic_create_int(int value);
SclDynamic     scl_dynamic_create_uint(unsigned int value);
SclDynamic     scl_dynamic_create_int8_t(int8_t value);
SclDynamic     scl_dynamic_create_int16_t(int16_t value);
SclDynamic     scl_dynamic_create_int32_t(int32_t value);
SclDynamic     scl_dynamic_create_int64_t(int64_t value);
SclDynamic     scl_dynamic_create_uint8_t(uint8_t value);
SclDynamic     scl_dynamic_create_uint16_t(uint16_t value);
SclDynamic     scl_dynamic_create_uint32_t(uint32_t value);
SclDynamic     scl_dynamic_create_uint64_t(uint64_t value);
SclDynamic     scl_dynamic_create_char(char value);
SclDynamic     scl_dynamic_create_uchar(unsigned char value);
SclDynamic     scl_dynamic_create_schar(signed char value);
SclDynamic     scl_dynamic_create_float(float value);
SclDynamic     scl_dynamic_create_double(double value);
SclDynamic     scl_dynamic_create_pair(pair_t pair);
SclDynamic     scl_dynamic_create_sequence(sequence_t pair);
void           scl_dynamic_free(SclDynamic value);


typedef void* SclAny;


typedef struct SclPairOpaque;
typedef SclPairOpaque* SclPair;

typedef struct SclSequenceOpaque;
typedef SclSequenceOpaque* SclSequence;



SclPair        scl_pair_create(SclAny* value, SclAny* value);
void           scl_pair_get_left(SclPair pair, SclAny* value);
void           scl_pair_get_right(SclPair pair, SclAny* value);
void           scl_pair_free(SclPair pair);

SclSequence    scl_sequence_empty();
SclSequence    scl_sequence_create(SclAny* value, SclSequence rest);
int            scl_sequence_is_empty(SclSequence value);
void           scl_sequence_get_first(SclSequence sequence, SclAny* value);
SclSequence    scl_sequence_get_rest(SclSequence sequence);
void           scl_sequence_free(SclSequence sequence);

void           scl_sequence_map(SclSequence sequence, void(*)(SclAny* ptr));
void           scl_sequence_fold(SclSequence sequence, SclAny* location, void(*)(SclAny* ptr, SclAny* res));




