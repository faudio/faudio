
#ifndef _DOREMIR_UTIL_ALIGNOF
#define _DOREMIR_UTIL_ALIGNOF

#ifndef alignof
#define alignof(T) offsetof (struct { char c; T member; }, member)
#endif

#endif // _DOREMIR_UTIL_ALIGNOF

