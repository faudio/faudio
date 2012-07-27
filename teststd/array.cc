
#include <array>
#include <iostream>

int array()
{
    std::array<int, 10> xs = { 1, 2, 3, 4 };
    for (int x : xs)
        std::cout << x << "\n";

    std::cout << "Size is : "     << xs.size()     << "\n";
    std::cout << "Max size is : " << xs.max_size() << "\n";
        
    return 0;
}   

int main (int argc, char const *argv[])
{
    return
       array()
    ;
}
