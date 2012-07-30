
// Implement something like this:
// http://members.shaw.ca/akochoi/articles/unicode-processing-c++0x/index.html

// inconv might not be necessary

// TODO what about the boost algorithms?
// overload (inefficiently?) by using converters to/from utf8?



#include <string>
#include <iostream>
#include <stdexcept>
#include <algorithm>

#include <boost/range/algorithm/transform.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/algorithm/string/join.hpp>
#include <boost/algorithm/string/case_conv.hpp>

// #include "Converter.h"
// #include "Converter.cc"

#include <iconv.h> // not supported on MSYS yet



template <class T, class Container = std::basic_string<T>>
class string_adapter
{
public:
    using container_type  = Container;
    using value_type      = typename container_type::value_type;
    using size_type       = typename container_type::size_type;
    using reference       = typename container_type::reference;
    using const_reference = typename container_type::const_reference;
    using iterator        = typename container_type::iterator;
    using const_iterator  = typename container_type::const_iterator;

    explicit string_adapter(container_type& str) : mStr(str) {}
    iterator       begin()       { return mStr.begin(); }
    iterator       end()         { return mStr.end();   }
    const_iterator begin() const { return mStr.begin(); }
    const_iterator end()   const { return mStr.end();   }
private:
    container_type& mStr;
};


template <class T>
struct value_type_of
{
    using type = typename T::value_type;
};

template <class T>
struct adapter_of
{                                           
    using type = string_adapter
        < typename value_type_of<T>::type
        , T 
        >;
};

template <class Str>
typename adapter_of<Str>::type unicode(Str& str)
{                       
    return typename adapter_of<Str>::type(str);
}


int adapter()
{
    std::u16string str = u"Hans Hoglund";
    std::u16string str2;
    str2.resize(str.size());

    // boost::transform(
    //     unicode(str),
    //     unicode(str2).begin(),
    //     [] (char16_t c){ return c + 0; });

    std::transform(
        unicode(str).begin(),
        unicode(str).end(),
        unicode(str2).begin(),
        [] (char16_t c){ return c + 1; });

    for (char c : str2)
        std::cout << c;
    std::cout << '\n';

    return 0;
}


int strs()
{
    std::u16string s;
    std::u32string t;

    s = u"hans";
    t = U"sven";

    // std:: cout << s.c_str() << "\n";
    // std:: cout << t << "\n";

    return 0;
}


////////////////



int algorithms()
{
    std::string x ("hans");
    std::string y;
    y.resize(x.size());
    boost::to_upper_copy(y.begin(), x);
    return 0;
}

int algorithms2()
{
    std::u16string a (u"hans");
    std::u16string b (u"sven");
    std::list<std::u16string> strs;
    strs.push_back(a);
    strs.push_back(b);

    std::u16string k = boost::algorithm::join(strs, ",");

    // std::u16string x16 (u"hans");
    // std::u16string y16;
    // boost::to_upper_copy(y16.begin(), x16);


    return 0;
}

////////////////


////////////////

int iconv()
{
    iconv_t x = iconv_open("ASCII", "ASCII");
    return 0;
}


int sizes()
{
    std::cout << "sizeof(char):     " << sizeof(char)     << "\n";
    std::cout << "sizeof(wchar_t):  " << sizeof(wchar_t)  << "\n";
    std::cout << "sizeof(char16_t): " << sizeof(char16_t) << "\n";
    std::cout << "sizeof(char32_t): " << sizeof(char32_t) << "\n";

    return 0;
}


int main (int argc, char const *argv[])
{
    return
         sizes()
       | adapter()
       | strs()
       | algorithms()
       | iconv()
    ;
}
