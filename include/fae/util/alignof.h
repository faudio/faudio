
#ifndef _FAE_UTIL_ALIGNOF
#define _FAE_UTIL_ALIGNOF

#ifndef alignof
#define alignof(T) offsetof (struct { char c; T member; }, member)
#endif

#endif // _FAE_UTIL_ALIGNOF

