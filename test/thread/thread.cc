
#include <gtest/gtest.h>
#include <scl/thread.hpp>

// Two threads with unique mutex

static scl::thread::mutex foo_bar_mutex;
void foo()
{
  for(int i = 0; i < 10; ++i)
  {
    scl::thread::lock_guard<scl::thread::mutex> foo_bar_lock (foo_bar_mutex);
    std::cout << "This is foo\n";
  }
}
void bar()
{
  for(int i = 0; i < 10; ++i)
  {
    scl::thread::lock_guard<scl::thread::mutex> foo_bar_lock (foo_bar_mutex);
    std::cout << "This is bar\n";
  }
}

TEST(Thread, Basic)
{
  scl::thread::thread x (foo);
  scl::thread::thread y (bar);
  x.join();
  y.join();
}

