
#include <gtest/gtest.h>
#include <scl/thread.hpp>

// Two threads with unique mutex

static scl::thread::mutex foo_bar_mutex;
void foo()
{
  using namespace scl::thread;
  for (int i = 0; i < 10; ++i)
  {
    lock_guard<mutex> foo_bar_lock(foo_bar_mutex);
    std::cout << "This is foo\n";
  }
}
void bar()
{
  using namespace scl::thread;
  for (int i = 0; i < 10; ++i)
  {
    lock_guard<mutex> foo_bar_lock(foo_bar_mutex);
    std::cout << "This is bar\n";
  }
}

TEST(Thread, Basic)
{
  using namespace scl::thread;
  thread x(foo);
  thread y(bar);
  x.join();
  y.join();
}

// Futures

// TEST(Thread, Future)
// {
// using namespace scl::thread;

// packaged_task<int()> task([](){return 7;});
// future<int> result = task.get_future();
// thread(move(task)).detach();
// cout << "Waiting...";
// result.wait();
// cout << "Done!\nResult is " << result.get() << '\n';
// }

