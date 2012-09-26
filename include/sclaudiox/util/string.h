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
#include <utility>   

#include <iconv.h>

#ifdef SCL_UNICODE
//    #ifdef SCL_WIN
//        #include "unicode/pwin32.h"
//    #endif
    #include "unicode/utypes.h"
    #include "unicode/unistr.h"
    #include "unicode/ustream.h"
    #include "unicode/stringpiece.h"
#endif       

#include "sclaudiox/util/foreach.h"

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

template <class T>
String toString(T value)
{
    std::stringstream ss;
    ss << value;
#ifdef SCL_UNICODE
    return UnicodeString(ss.str().c_str());
#else
    return ss.str();
#endif
}
        
template <class T>
String toString(std::list<T> lst)
{
    typedef typename std::list<T>   List;
    typedef typename List::iterator Iterator;

    String buffer;
    buffer += "(";
    buffer += " ";
    for (Iterator it = lst.begin(); it != lst.end(); ++it) 
    {
        buffer += toString(*it);
        buffer += " ";
    }
    buffer += ")";

    return buffer;
}     

template <class A, class B>
String toString(std::pair<A,B> pair)
{
    return "{" + toString<A>(pair.first) + "," + toString<B>(pair.second) + "}";
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

template <>
inline String fromSimpleString<kDefaultCharSet>(const char* str)
{
    return UnicodeString(str, ""); // FIXME is this right?
}

#endif // SCL_UNICODE

typedef String FilePath;

} // namespace
} // namespace

#endif
