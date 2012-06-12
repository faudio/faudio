#ifndef HS_AUDIOENGINE_H_HSC_H
#define HS_AUDIOENGINE_H_HSC_H
#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ < 409
#include <Rts.h>
#endif
#include <HsFFI.h>
#if __NHC__
#undef HsChar
#define HsChar int
#endif
#line 2 "HS_AUDIOENGINE_H.hsc"
#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ < 409
#line 3 "HS_AUDIOENGINE_H.hsc"
#include <Rts.h>
#line 4 "HS_AUDIOENGINE_H.hsc"
#endif 
#line 5 "HS_AUDIOENGINE_H.hsc"
#include <HsFFI.h>
#line 7 "HS_AUDIOENGINE_H.hsc"
#include <stddef.h>
#line 8 "HS_AUDIOENGINE_H.hsc"
#include <string.h>
#line 9 "HS_AUDIOENGINE_H.hsc"
#include <stdio.h>
#line 10 "HS_AUDIOENGINE_H.hsc"
#include <stdarg.h>
#line 11 "HS_AUDIOENGINE_H.hsc"
#include <ctype.h>
#line 13 "HS_AUDIOENGINE_H.hsc"
#ifndef __quote__
#line 14 "HS_AUDIOENGINE_H.hsc"
#define __quote__(x...) x
#line 15 "HS_AUDIOENGINE_H.hsc"
#endif 
#line 17 "HS_AUDIOENGINE_H.hsc"
#ifndef offsetof
#line 18 "HS_AUDIOENGINE_H.hsc"
#define offsetof(t, f) ((size_t) &((t *)0)->f)
#line 19 "HS_AUDIOENGINE_H.hsc"
#endif 
#line 22 "HS_AUDIOENGINE_H.hsc"
#if __NHC__
#line 23 "HS_AUDIOENGINE_H.hsc"
#define hsc_line(line, file)     printf ("# %d \"%s\"\n", line, file);
#line 25 "HS_AUDIOENGINE_H.hsc"
#else 
#line 26 "HS_AUDIOENGINE_H.hsc"
#define hsc_line(line, file)     printf ("{-# LINE %d \"%s\" #-}\n", line, file);
#line 28 "HS_AUDIOENGINE_H.hsc"
#endif 
#line 30 "HS_AUDIOENGINE_H.hsc"
#define hsc_const(x)                            if ((x) < 0)                                    printf ("%ld", (long)(x));              else                                            printf ("%lu", (unsigned long)(x));
#line 36 "HS_AUDIOENGINE_H.hsc"
#define hsc_const_str(x)                                              {                                                                     const char *s = (x);                                              printf ("\"");                                                    while (*s != '\0')                                                {                                                                     if (*s == '"' || *s == '\\')                                          printf ("\\%c", *s);                                          else if (*s >= 0x20 && *s <= 0x7E)                                    printf ("%c", *s);                                            else                                                                  printf ("\\%d%s",                                                         (unsigned char) *s,                                               s[1] >= '0' && s[1] <= '9' ? "\\&" : "");             ++s;                                                          }                                                                 printf ("\"");                                                }
#line 55 "HS_AUDIOENGINE_H.hsc"
#define hsc_type(t)                                             if ((t)(int)(t)1.4 == (t)1.4)                                   printf ("%s%d",                                                     (t)(-1) < (t)0 ? "Int" : "Word",                            sizeof (t) * 8);                                else                                                            printf ("%s",                                                       sizeof (t) >  sizeof (double) ? "LDouble" :                 sizeof (t) == sizeof (double) ? "Double"  :                 "Float");
#line 66 "HS_AUDIOENGINE_H.hsc"
#define hsc_peek(t, f)     printf ("(\\hsc_ptr -> peekByteOff hsc_ptr %ld)", (long) offsetof (__quote__(t), f));
#line 69 "HS_AUDIOENGINE_H.hsc"
#define hsc_poke(t, f)     printf ("(\\hsc_ptr -> pokeByteOff hsc_ptr %ld)", (long) offsetof (__quote__(t), f));
#line 72 "HS_AUDIOENGINE_H.hsc"
#define hsc_ptr(t, f)     printf ("(\\hsc_ptr -> hsc_ptr `plusPtr` %ld)", (long) offsetof (__quote__(t), f));
#line 75 "HS_AUDIOENGINE_H.hsc"
#define hsc_offset(t, f)     printf("(%ld)", (long) offsetof (__quote__(t), f));
#line 78 "HS_AUDIOENGINE_H.hsc"
#define hsc_size(t)     printf("(%ld)", (long) sizeof(t));
#line 81 "HS_AUDIOENGINE_H.hsc"
#define hsc_enum(t, f, print_name, x)             print_name;                                   printf (" :: %s\n", #t);                      print_name;                                   printf (" = %s ", #f);                        if ((x) < 0)                                      printf ("(%ld)\n", (long)(x));            else                                              printf ("%lu\n", (unsigned long)(x));
#line 91 "HS_AUDIOENGINE_H.hsc"
#define hsc_haskellize(x)                                              {                                                                      const char *s = (x);                                               int upper = 0;                                                     if (*s != '\0')                                                    {                                                                      putchar (tolower (*s));                                            ++s;                                                               while (*s != '\0')                                                 {                                                                      if (*s == '_')                                                         upper = 1;                                                     else                                                               {                                                                      putchar (upper ? toupper (*s) : tolower (*s));                     upper = 0;                                                     }                                                                  ++s;                                                           }                                                              }                                                              }
#line 113 "HS_AUDIOENGINE_H.hsc"
extern void _dummy_force_HS_AUDIOENGINE_H_hsc_c (void) ;
#line 119 "HS_AUDIOENGINE_H.hsc"
#include "hs_audioengine.h"
#line 120 "HS_AUDIOENGINE_H.hsc"
#ifndef __quote__
#line 121 "HS_AUDIOENGINE_H.hsc"
#define __quote__(x...) x
#line 122 "HS_AUDIOENGINE_H.hsc"
#endif 
#line 382 "HS_AUDIOENGINE_H.hsc"
#include <stdlib.h>
#endif
