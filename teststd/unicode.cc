
// Implement something like this:
// http://members.shaw.ca/akochoi/articles/unicode-processing-c++0x/index.html

// inconv might not be necessary

// TODO what about the boost algorithms?
// overload (inefficiently?) by using converters to/from utf8?



#include <string>
#include <iostream>
#include <stdexcept>
#include <algorithm>
#include <array>

#include <boost/range/algorithm/copy.hpp>
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

    boost::transform(
        unicode(str),
        unicode(str2).begin(),
        [] (char16_t c){ return c + 10; });

    // std::transform(
    //     unicode(str2).begin(),
    //     unicode(str2).end(),
    //     unicode(str2).begin(),
    //     [] (char16_t c){ return c + 1; });

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

namespace range 
{
    using boost::copy;
}
using std::copy;



template < class SinglePassRange
         , class OutputIterator >
void iconv_range(const SinglePassRange& input,
                       OutputIterator   output,
                       iconv_t          converter)
{        
    // TODO some concept check to see that range::value_type is convertible to char
    size_t size    = boost::size(input);
    size_t insize  = sizeof(char) * size;
    size_t outsize = insize;

    std::unique_ptr<char> inbuf  (new char[size]);
    std::unique_ptr<char> outbuf (new char[size]);
    char *inbuf_ptr  = inbuf.get();
    char *outbuf_ptr = outbuf.get();

    range::copy(input, inbuf.get());
    size_t conv_result = iconv(converter, &inbuf_ptr, &insize, &outbuf_ptr, &outsize);
    copy(outbuf.get(), outbuf.get() + size, output);
}


// template < class SinglePassRange
//          , class OutputIterator >
// bool iconv_range(const SinglePassRange& input,
//                        OutputIterator   output,
//                        iconv_t          converter)
// {        
//     // TODO some concept check to see that range::value_type is convertible to char
//     size_t size = boost::size(input);
//     size_t insize  = sizeof(char) * size;
//     size_t outsize = insize;
// 
//     char *inbuf2,  *inbuf;  
//     char *outbuf2, *outbuf;
//     inbuf  = inbuf2  = new char[size];
//     outbuf = outbuf2 = new char[size];
// 
//     range::copy(input, inbuf);
//     size_t conv_result = iconv(converter, &inbuf2, &insize, &outbuf2, &outsize);
//     copy(outbuf, outbuf + size, output);
// 
//     delete [] inbuf;
//     delete [] outbuf;
//     return true;
// }


int test_iconv_range2()
{
//    std::array<char,4> x = { 'h', 'a', 'n', 's' };
    char x[5] = "hans";
    std::string y;
    y.resize(4);

    iconv_t cd = iconv_open("ASCII", "MacRoman");
    if (cd == reinterpret_cast<iconv_t>(-1))
    {
        std::cerr << "Could not open converter\n";
        return -1;
    }
    iconv_range(x, y.begin(), cd);
    // std::cout << "x is " << x << "\n";
    std::cout << "y is " << y << "\n";
    return 0;
}


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
       | test_iconv_range2()
    ;
}
