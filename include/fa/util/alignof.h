
#ifndef _FA_UTIL_ALIGNOF
#define _FA_UTIL_ALIGNOF

#ifndef alignof
#define alignof(T) offsetof (struct { char c; T member; }, member)
#endif

#endif // _FA_UTIL_ALIGNOF

