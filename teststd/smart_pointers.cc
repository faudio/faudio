
#include <iostream>
#include <memory>


template <char Char>
class named
{
public:
  named() 
  {
    std::cout << Char << " created\n";
  }
  ~named()
  {
    std::cout << Char << " released\n";
  }
  std::string name() const
  {
    if (!this) throw;
    const char str[2] = { Char, 0 };
    return std::string(str);
  }
};

template <char Char>
std::ostream& operator <<(std::ostream &os, const named<Char> &obj)
{
    os << obj.name();
    return os;
}

using a = named<'a'>;
using b = named<'b'>;
using c = named<'c'>;
// using a_ptr = std::unique_ptr<a>;
// using b_ptr = std::unique_ptr<b>;
// using c_ptr = std::unique_ptr<c>;

int test_unique_ptr()
{
  std::unique_ptr<a> x (new a);
  std::unique_ptr<a> y;
  
  std::cout << "x has address " << x.get()  << "\n";
  std::cout << "x is named "  << *x.get() << "\n";
  std::cout << "y has address " << y.get()  << "\n";
  // std::cout << "y is named "  << *y.get() << "\n";

  y = std::move(x);

  std::cout << "x has address " << x.get() << "\n";
  // std::cout << "x is named "  << *x.get() << "\n";
  std::cout << "y has address " << y.get()  << "\n";
  std::cout << "y is named "  << *y.get() << "\n";
  
  return 0;
}   

int main(int argc, char const *argv[])
{
  return
     test_unique_ptr()
  ;
}
