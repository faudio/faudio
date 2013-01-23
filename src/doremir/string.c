
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/string.h>
#include <doremir/util.h>
#include <iconv.h>

#include <CoreFoundation/CoreFoundation.h> // TODO OS X only

/*
    Notes:
        * Straightforward implementation using bounded buffer
        * Slow copying
        * Reasonable dappend (using realloc)
        * Using iconv for conversion
            * Constantly open/close for reentrant conversion
                * Can we pool iconv instances instead?
                * Would require queue/mutex to syncronize access to the converters
                * Is iconv reset reliable?

    Possibilities:
        * More efficient append
            * Separate size from char count and pre-allocate (such as 2^n).
        * Real-time alternative?
            * Non-amortized allocator?
            * Regional string buffer/symbol table solution?
 */

#define kstd_code   "UTF-16LE"          /* Internal string code */
#define kchar_size  sizeof(uint16_t)    /* Internal char size */

struct _doremir_string_t {
  impl_t          impl;
  size_t          size;
  uint16_t         *data;
};

doremir_ptr_t string_impl(doremir_id_t interface);
static void fatal(char *msg, int error);

string_t new_string(size_t size, uint16_t *data)
{
  string_t str = doremir_new(string);

  str->impl = &string_impl;
  str->size = size;
  str->data = data;

  return str;
}

void delete_string(string_t str)
{
  doremir_delete(str);
}

// --------------------------------------------------------------------------------

/** Create an empty string.

    The returned string should be destroyed by the caller.
 */
doremir_string_t doremir_string_empty()
{
  return new_string(0, NULL);
}

/** Create a single-char string.

    The returned string should be destroyed by the caller.
 */
doremir_string_t doremir_string_single(uint16_t chr)
{
  string_t str = new_string(1, NULL);
  str->data = malloc(kchar_size);
  str->data[0] = chr;

  return str;
}

/** Copy the given string.

    The returned string should be destroyed by the caller.
 */
doremir_string_t doremir_string_copy(doremir_string_t str)
{
  string_t pst = new_string(str->size, NULL);
  pst->data = malloc(str->size * kchar_size);

  memcpy(pst->data, str->data, str->size * kchar_size);

  return pst;
}

/** Append the given strings.

    The returned string should be destroyed by the caller.
 */
doremir_string_t doremir_string_append(doremir_string_t as,
                                       doremir_string_t bs)
{
  string_t cs = new_string(as->size + bs->size, NULL);
  cs->data = malloc(cs->size * kchar_size);

  memcpy(cs->data, as->data, as->size * kchar_size);
  memcpy(cs->data + as->size, bs->data, bs->size * kchar_size);

  return cs;
}

/** Append the given strings, which are both destroyed.

    The returned string should be destroyed by the caller.
 */
doremir_string_t doremir_string_dappend(doremir_string_t as,
                                        doremir_string_t bs)
{
  size_t oldSize = as->size;

  as->size = as->size + bs->size;
  as->data = realloc(as->data, as->size * kchar_size);

  memcpy(as->data + oldSize, bs->data, bs->size * kchar_size);

  free(bs);
  return as;
}

/** Destroy the given string.
 */
void doremir_string_destroy(doremir_string_t str)
{
  free(str->data);
  delete_string(str);
}


// --------------------------------------------------------------------------------
// Access and predicates
// --------------------------------------------------------------------------------

/** Return the number of characters in the given string.
 */
int doremir_string_length(doremir_string_t str)
{
  return str->size;
}

/** Return the character at the given position in the string.
 */
uint16_t doremir_string_char_at(int n, doremir_string_t str)
{
  if (n < 0 || n >= str->size) {
    assert(false && "Out of range");
  }

  return str->data[n];
}


// --------------------------------------------------------------------------------
// Formatting
// --------------------------------------------------------------------------------

/** Format an integer.
    @param format
        A printf-style format string.
    @param value
        Integer value.
    @return
        A new formatted string.
 */
doremir_string_t doremir_string_format_integer(char *format, long value)
{
  char buffer[100];
  int  numChars;

  numChars = snprintf(buffer, 100, format, value);

  if (numChars > 100) {
    fatal("Too many characters", -1);
  }

  buffer[numChars] = 0;
  return doremir_string_from_utf8(buffer);
}

// --------------------------------------------------------------------------------
// Conversion
// --------------------------------------------------------------------------------

/** Fail with error message, interpreting errno as an iconv error.

    This function does not return.
 */
static inline void iconv_fail()
{
  switch (errno) {
  case E2BIG:
    fatal("iconv: Output buffer too small",
          errno);

  case EILSEQ:
    fatal("iconv: Input byte does not belong to the input codeset",
          errno);

  case EINVAL:
    fatal("iconv: Incomplete character or shift sequence at the end of the input buffer",
          errno);

  default:
    fatal("iconv: Unknown error",
          errno);
  }
}

static inline size_t raw_size(char *s)
{
  size_t i = 0;

  while (s[i]) {
    i++;
  }

  return i;
}

static inline size_t raw_size_16(uint16_t *s)
{
  size_t i = 0;

  while (s[i]) {
    i++;
  }

  return i;
}

/** Encode the given string as UTF-8.

    @param  str String to encode.
    @return
        A heap-allocated encoded string.
 */
doremir_string_utf8_t doremir_string_to_utf8(doremir_string_t str)
{
  size_t inSize, outSize, cstrSize;
  char *in, *out, *cstr;

  inSize  = str->size * kchar_size;   // exact char count
  outSize = str->size * 4;            // worst case, we shrink after iconv
  in      = (char *) str->data;
  out     = malloc(outSize);
  cstr    = out;

  {
    iconv_t conv   = iconv_open("UTF-8", kstd_code);
    size_t  status = iconv(conv, &in, &inSize, &out, &outSize);
    iconv_close(conv);

    if (status < 0) {
      iconv_fail();
    }
  }

  cstrSize = out - cstr;
  cstr     = realloc(cstr, cstrSize + 1);

  cstr[cstrSize] = 0;                 // add null-terminator

  return cstr;
}

/** Encode the given string as UTF-16.

    @param  str String to encode.
    @return
        A heap-allocated encoded string.
 */
doremir_string_utf16_t doremir_string_to_utf16(doremir_string_t as)
{
  size_t size = as->size;
  uint16_t *cstr = malloc((size + 1) * kchar_size);
  memcpy(cstr, as->data, as->size * kchar_size);
  cstr[size] = 0;
  return cstr;
}

/** Encode the given string as UTF-32.

    @param  str String to encode.
    @return
        A heap-allocated encoded string.
 */
doremir_string_utf32_t doremir_string_to_utf32(doremir_string_t str)
{
  assert(false && "Not implemented");
}

/** Deencode a string from UTF-8.

    @param  str Encoded string.
    @return
        A new string.
 */
doremir_string_t doremir_string_from_utf8(doremir_string_utf8_t cstr)
{
  size_t inSize, outSize, strSize;
  char *in, *out, *str;

  inSize  = raw_size(cstr);    // char count is in [inSize/4,inSize]
  outSize = inSize * 2;        // worst case, we shrink after iconv
  in      = cstr;
  out     = malloc(outSize);
  str     = out;

  {
    iconv_t conv = iconv_open(kstd_code, "UTF-8");
    size_t status = iconv(conv, &in, &inSize, &out, &outSize);
    iconv_close(conv);

    if (status < 0) {
      iconv_fail();
    }
  }

  strSize = out - str;
  str     = realloc(str, strSize);

  return new_string(strSize / kchar_size, (uint16_t *) str);
}

/** Deencode a string from UTF-16.

    @param  str Encoded string.
    @return
        A new string.
 */
doremir_string_t doremir_string_from_utf16(doremir_string_utf16_t cstr)
{
  size_t size = raw_size_16(cstr);
  string_t as = new_string(size, malloc(size * kchar_size));
  memcpy(cstr, as->data, as->size * kchar_size);
  return as;
}

/** Deencode a string from UTF-32.

    @param  str Encoded string.
    @return
        A new string.
 */
doremir_string_t doremir_string_from_utf32(doremir_string_utf32_t cstr)
{
  assert(false && "Not implemented");
}


// --------------------------------------------------------------------------------

// TODO OS X only

/** Encode a string as a CFString.

    @note   OS X only
    @return
        A new CFStringRef.
 */
void *doremir_string_to_cf_string(doremir_string_t str)
{
  char *cstr;
  CFStringRef cfstr;

  cstr    = doremir_string_to_utf8(str);
  cfstr   = CFStringCreateWithCString(kCFAllocatorDefault, cstr, kCFStringEncodingUTF8);

  free(cstr);
  return (void *) cfstr;
}

/** Deencode a string from a CFString.

    @note   OS X only
    @param cfstr
        A CFStringRef.
    @return
        A new string.
 */
doremir_string_t doremir_string_from_cf_string(void *cfstr)
{
  CFIndex size;
  char *cstr;
  string_t str;

  if ((cstr = (char *) CFStringGetCStringPtr(cfstr, kCFStringEncodingUTF8))) {
    return doremir_string_from_utf8(cstr);
  } else {
    size        = CFStringGetLength(cfstr);
    cstr        = malloc(size + 1);
    cstr[size]  = 0;                     // necesary ?

    CFStringGetCString(cfstr, cstr, size + 1, kCFStringEncodingUTF8);
    str = doremir_string_from_utf8(cstr);

    free(cstr);
    return str;
  }
}


// --------------------------------------------------------------------------------

/** Generic version of the @ref doremir_string_show_t interface.

    Ideally, this should be in the @ref Doremir module, but this would create a recursive
    dependency.

    @param a
        Value to convert.
    @return
        A new string created from the given value.
 */
doremir_string_t doremir_string_show(doremir_ptr_t a)
{
  return ((doremir_string_show_t *) doremir_interface(doremir_string_show_i, a))->show(a);
}



// --------------------------------------------------------------------------------

static bool string_equal(doremir_ptr_t as, doremir_ptr_t bs)
{
  string_t cs, ds;
  cs = (string_t) as;
  ds = (string_t) bs;

  if (cs->size != ds->size) {
    return false;
  } else {
    for (size_t i = 0;
         i < cs->size && i < ds->size;
         ++i) {
      if (cs->data[i] != ds->data[i]) {
        return false;
      }
    }

    return true;
  }
}

#define pred(a) (a - 1)
#define min(a,b) ((a < b) ? a : b)
#define last_elem(v) v->data[pred(v->size)]

static bool string_less_than(doremir_ptr_t as, doremir_ptr_t bs)
{
  string_t cs, ds;
  cs = (string_t) as;
  ds = (string_t) bs;

  for (size_t i;
       i < pred(min(cs->size, ds->size));
       ++i) {
    if (cs->data[i] < ds->data[i]) {
      return true;
    }

    if (cs->data[i] > ds->data[i]) {
      return false;
    }
  }

  if (cs->size == ds->size) {
    return last_elem(cs) < last_elem(ds);
  } else {
    return (cs->size < ds->size);
  }
}

static bool string_greater_than(doremir_ptr_t as, doremir_ptr_t bs)
{
  string_t cs, ds;
  cs = (string_t) as;
  ds = (string_t) bs;

  for (size_t i;
       i < pred(min(cs->size, ds->size));
       ++i) {
    if (cs->data[i] > ds->data[i]) {
      return true;
    }

    if (cs->data[i] < ds->data[i]) {
      return false;
    }
  }

  if (cs->size == ds->size) {
    return last_elem(cs) > last_elem(ds);
  } else {
    return (cs->size > ds->size);
  }
}


static doremir_string_t string_show(doremir_ptr_t a)
{
  // TODO proper escaping
  string_t s = string("");
  s = doremir_string_dappend(s, string("\""));
  s = doremir_string_dappend(s, doremir_string_copy(a));
  s = doremir_string_dappend(s, string("\""));
  return s;
}


doremir_ptr_t string_copy(doremir_ptr_t a)
{
  return doremir_string_copy(a);
}

void string_destroy(doremir_ptr_t a)
{
  doremir_string_destroy(a);
}

doremir_ptr_t string_impl(doremir_id_t interface)
{
  static doremir_equal_t string_equal_impl = { string_equal };
  static doremir_copy_t string_copy_impl = { string_copy };
  static doremir_string_show_t string_show_impl = { string_show };
  static doremir_destroy_t string_destroy_impl = { string_destroy };
  static doremir_order_t string_order_impl = { string_less_than, string_greater_than };

  switch (interface) {
  case doremir_equal_i:
    return &string_equal_impl;

  case doremir_order_i:
    return &string_order_impl;

  case doremir_string_show_i:
    return &string_show_impl;

  case doremir_copy_i:
    return &string_copy_impl;

  case doremir_destroy_i:
    return &string_destroy_impl;

  default:
    return NULL;
  }
}


// --------------------------------------------------------------------------------

static void fatal(char *msg, int error)
{
  printf("Fatal error: Doremir: String: %s: %d\n", msg, error);
  exit(error);
}

