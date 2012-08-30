
#include <gtest/gtest.h>
#include <scl/utility.hpp>
#include <scl/reserve_ptr.hpp>

using std::shared_ptr;
using std::unique_ptr;
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
  static const unique_ptr<reserve_context> context { create_reserve_context() };

  shared_ptr<foo> x { new foo() };
  shared_ptr<bar> y { new bar() };
  shared_ptr<foo> x2 (x);

  reserve_ptr(x, context.get());
  reserve_ptr(y, context.get());
  unreserve_ptr(x2, context.get());
  unreserve_ptr(y, context.get());
}

TEST(AudioUtility, ReservePtrGet)
{
  static const unique_ptr<reserve_context> context { create_reserve_context() };

  shared_ptr<foo> x { new foo() };
  reserve_ptr(x, context.get());

  shared_ptr<foo> y = get_reserved_ptr<foo>(x.get(), context.get());
  shared_ptr<foo> z = get_reserved_ptr<foo>(nullptr, context.get());
  unreserve_ptr(x, context.get());

  std::cout << !!x << "\n";
  std::cout << !!y << "\n";
  std::cout << !!z << "\n";
}
