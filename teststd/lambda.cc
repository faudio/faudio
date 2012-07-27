
#include <iostream>
#include <list>
#include <boost/range/algorithm/transform.hpp>
#include <boost/range/algorithm/equal.hpp>

int hello()
{
    auto foo = [] () { std::cout << "Hello world\n"; };
    foo();
    return 0;
}

int mapping()
{    
    auto succ = [] (int x) -> int { return x + 1; };
    auto pred = [] (int x) -> int { return x - 1; };

    std::list<int> xs, ys, zs;

    xs = { 1, 2, 3 };

    ys.resize(xs.size());
    zs.resize(xs.size());
    boost::transform(xs, ys.begin(), succ);
    boost::transform(ys, zs.begin(), pred);

    for (int x : xs) std::cout << " " << x;
    std::cout << "\n";
    for (int y : ys) std::cout << " " << y;
    std::cout << "\n";
    for (int z : zs) std::cout << " " << z;
    std::cout << "\n";
        
    // assert(boost::equal(xs, zs));
    
    return 0;
}

int capture()
{
    auto foo = [] (int x, int y) {
        return [=] () -> int {
            return x + y;
        };
    };
    auto bar = foo(1, 2);
    
    std::cout << "Lazy value: " << bar();
    std::cout << "\n";
    return 0;
}

int main (int argc, char const *argv[])
{
    return
       hello()
    || mapping()
    || capture()
    ;
}
