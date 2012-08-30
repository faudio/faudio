
#include <gtest/gtest.h>
#include <scl/utility.hpp>
#include <scl/reserve_ptr.hpp>

using std::shared_ptr;
using namespace scl;

struct foo
{
  char data [10999];
  foo() { std::cout  << "Creating foo " << this << "!\n"; }
  ~foo() { std::cout  << "Destroying foo " << this << "!\n"; }
};
struct bar
{                                       
  char data [20000];
  bar() { std::cout  << "Creating bar " << this << "!\n"; }
  ~bar() { std::cout  << "Destroying bar " << this << "!\n"; }
};


TEST(AudioUtility, ReservePtr)
{
  static reserve_context_ptr context = create_reserve_context();

  shared_ptr<foo> x { new foo() };
  shared_ptr<bar> y { new bar() };

  reserve_ptr(x, context);
  reserve_ptr(y, context);
  // unreserve_ptr(x, context);
  destroy_reserve_context(context);
}
