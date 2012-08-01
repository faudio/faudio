
#include <iostream>
#include <exception>
#include "thread.h"

class foo_exception : public std::exception 
{
  const char* what() const noexcept override  
  {
    return "Foo &/#!!";
  }
};
class bar_exception : public std::exception 
{
  const char* what() const noexcept override  
  {
    return "Bar &/#!!";
  }
};
static exception::exception_ptr foo_exception_ptr;
static exception::exception_ptr bar_exception_ptr;


void foo()
{
  std::cout << "This is foo!\n";
  try
  {
    throw foo_exception();
  }
  catch (const std::exception& e)
  {
    foo_exception_ptr = exception::current_exception();
  }
}

void bar()
{
  std::cout << "This is bar!\n";
  try
  {
    throw bar_exception();
  }
  catch (const std::exception& e)
  {                 
    bar_exception_ptr = exception::current_exception();
  }
}


void handle_eptr(std::string msg, exception::exception_ptr e)
{
  try
  {
    if (e)
    {
      exception::rethrow_exception(e);
    }
  }
  catch (const std::exception& e)
  {
    std::cout << msg << " \"" << e.what() << "\"\n";
  }
}

int start_foo_bar()
{
  thread::thread foo_thread(foo);
  thread::thread bar_thread(bar);
  foo_thread.join();
  bar_thread.join();
  
  handle_eptr("Exception in foo:", foo_exception_ptr);
  handle_eptr("Exception in bar:", bar_exception_ptr);

  std::cout << "Exiting normally\n";
  return 0;
}


int main(int argc, char const* argv[])
{
  return 0
    || start_foo_bar()
    ;
}
