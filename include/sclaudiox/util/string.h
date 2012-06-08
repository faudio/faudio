/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#ifndef _SCLAUDIOX_UTIL_STRING
#define _SCLAUDIOX_UTIL_STRING

#include <list>
#include <string>
#include <sstream>
#include <iostream>

#ifdef SCL_UNICODE
    #ifdef SCL_WIN
        #include "unicode/pwin32.h"
    #endif
    #include "unicode/utypes.h"
    #include "unicode/unistr.h"
    #include "unicode/ustream.h"
    #include "unicode/stringpiece.h"
#endif

namespace doremir {
namespace scl {

/**
    The String type is used for all string handling in the audio engine.

    If the SCL_UNICODE flag is set, this type corresponds to a Unicode string, 
    otherwise it corresponds to an Ascii string.
 */
#ifdef SCL_UNICODE
typedef UnicodeString String;
#else
typedef std::string   String;
#endif  
  
// Dummy to keep track of which parts of ICU interface we use

/*
class String
{
public:
    String& operator+= (const String& s){}
    int length() {}
    String(){}
    String(const char*){}

    String(icu_48::UnicodeString){}
    void extract( char*, Int32, UConverter*, UErrorCode& ){}
    void extract(UChar*, int32_t&, UErrorCode&){}
};
inline String& operator+ (const String& x, const String& y){}
inline bool    operator< (const String& x, const String& y){}
inline std::ostream& operator<< (const std::ostream& o, const String& s){}
*/


/**
    Converts the given value to a String.

    The default implementation can be used for all built-in types including 
    numbers, characters and strings.
 */
template <class Type>
String toString(Type value)
{
    std::stringstream ss;
    ss << value;
#ifdef SCL_UNICODE
    return UnicodeString(ss.str().c_str());
#else
    return ss.str();
#endif
}

/**
    Converts the given list to a String.

    Returns a String containing the result of converting all elements of the
    list to strings, separated by a space character and enclosed in brackets.
 */
template <class Element, class Iterator>
String toString(std::list<Element> lst)
{
    String buffer;

    buffer += "(";
    for (Iterator i = lst.begin(); i != lst.end(); ++i) 
    {
        buffer += toString(*lst);
        buffer += " ";
    }
    buffer += ")";

    return buffer;
}

enum CharacterSet
{
    kUtf8,
    kIso885915,
    kDefaultCharSet
};

/**
    Converts the given String to a C-style string.
    Non-Ascii characters will be stripped
 */
inline const char* toSimpleString(String str)
{
#ifdef SCL_UNICODE
    UErrorCode err = U_ZERO_ERROR;
    int32_t size = str.length() + 1;
    
    char* dest = new char[size];
    str.extract(dest, size, NULL, err);
    return dest;
#else
    return str.c_str();
#endif
}

template <CharacterSet Source>
inline String fromSimpleString(const char* str)
{
#ifdef SCL_UNICODE
    return UnicodeString(str);
#else
    return std::string(str);
#endif
}


#ifdef SCL_UNICODE
template <>
inline String fromSimpleString<kUtf8>(const char* str)
{
    return UnicodeString::fromUTF8(StringPiece(str));
}

template <>
inline String fromSimpleString<kIso885915>(const char* str)
{
    return UnicodeString(str, "ISO-8859-15");
}
#endif // SCL_UNICODE

typedef String FilePath;

} // namespace
} // namespace

#endif
