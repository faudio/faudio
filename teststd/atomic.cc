
#include <iostream>
#include "atomic.h"

int test_atomic()
{
  atomic_int x;
  x.store(1);
  std::cout << x.load() << "\n";

  return 0;
}   

int main(int argc, char const *argv[])
{
  return 0
     || test_atomic()
  ;
}
