
// Implement something like this:
// http://members.shaw.ca/akochoi/articles/unicode-processing-c++0x/index.html

// inconv might not be necessary

// TODO what about the boost algorithms?
// overload (inefficiently?) by using converters to/from utf8?

#include <iconv.h>

#include <string>
#include <iostream>
#include <stdexcept>
#include <boost/algorithm/string.hpp>
#include <boost/algorithm/string/join.hpp>
#include <boost/algorithm/string/case_conv.hpp>


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
#define STR std::u16string
    // STR x (L"hans");
    // STR y;
    // y.resize(x.size());
    // boost::to_upper_copy(y.begin(), x);
    // std::cout << y;
    // std::cout << "\n";

    STR a (u"hans");
    STR b (u"sven");
    std::list<STR> strs;
    strs.push_back(a);
    strs.push_back(b);

    STR k = boost::algorithm::join(strs, ",");

    // std::u16string x16 (u"hans");
    // std::u16string y16;
    // boost::to_upper_copy(y16.begin(), x16);


    return 0;
}

////////////////


////////////////

void convert(std::string in,    std::string& out);
void convert(std::u16string in, std::u16string& out);
void convert(std::u32string in, std::u32string& out);


void convert(std::string in, std::string& out)
{
    std::copy(in.begin(), in.end(), out.end());
}

void convert(std::u16string in, std::u16string& out) {}
void convert(std::u32string in, std::u32string& out) {}



// ((char*, size_t) -> (char*, size_t)) -> (CharRange -> CharRange)


////////////////

int iconv()
{
    iconv_t x = iconv_open("ASCII", "ASCII");
    return 0;
}




int main (int argc, char const *argv[])
{
    return
         strs()
       | algorithms()
       | iconv()
    ;
}
