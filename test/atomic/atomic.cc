
#include <gtest/gtest.h>
#include <scl/atomic.hpp>

TEST(Atomic, Basic)
{
  scl::atomic::atomic_int x;
  scl::atomic::atomic_int y;
  x.store(1);
  int _1 = x.load();
  int _2 = x.exchange(3);
  std::cout << x.load() << "\n";
}

TEST(Atomic, CompareAndExchange)
{
  scl::atomic::atomic_int x;
  int m = 0;
  x.compare_exchange_weak(m, 1);
  x.compare_exchange_strong(m, 1);
}

TEST(Atomic, Arithmetic)
{
  int p;
  int q;
  scl::atomic::atomic<int*> x(&p);
  x.fetch_add(&p - &q);
  x.fetch_sub(&p - &q);
}

TEST(Atomic, Flags)
{
  scl::atomic::atomic_flag q;
  bool _1 = q.test_and_set(); // set to 1 and return previous
  q.clear();                  // set to 0
}

