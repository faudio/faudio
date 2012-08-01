
#include <iostream>
#include <initializer_list>

template <class T>
struct foo
{
  foo(std::initializer_list<T> args)
  {
    std::cout << "Called with " << args.size() << " uniform arguments.\n";
    for(T arg : args)
      std::cout << "  " << arg << "\n";
  }
  foo(int x, double y, const char* z)
  {
    std::cout << "Called with 3 different arguments.\n";
    std::cout << "  " << x << "\n";
    std::cout << "  " << y << "\n";
    std::cout << "  " << z << "\n";
  }
};


int test_init_list()
{
  foo<int> xs = { 1, 2, 3 };
  foo<int> ys = { 1, 2, "My name is Hans!" };
}   

int main(int argc, char const *argv[])
{
  return 0
     || test_init_list()
  ;
}
