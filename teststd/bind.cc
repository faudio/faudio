
#include <functional>
#include <iostream>



int main (int argc, char const *argv[])
{
    auto add   = std::plus<int>();
    
    auto add1a = std::bind(add, 1, std::placeholders::_1);
    auto add1b = [] (int x) -> int { return x + 1; };

    std::cout << add1a(2) << "\n";
    std::cout << add1b(2) << "\n";
    
    return 0;
}
