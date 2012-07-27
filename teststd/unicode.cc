
#include <string>
#include <iostream>
#include <stdexcept>
#include <boost/algorithm/string.hpp>

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

int main (int argc, char const *argv[])
{
    return
       strs()
    ;
}
