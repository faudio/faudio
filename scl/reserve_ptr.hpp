
#pragma once

#include <map>
#include <memory>
#include <scl/thread.hpp>

namespace scl 
{
  struct reserve_context;
  typedef reserve_context* reserve_context_ptr;

  inline reserve_context_ptr create_reserve_context();
  inline void destroy_reserve_context(reserve_context_ptr ref);

  /** Prevent ptr from being deleted as long as reserve_context exists. */
  template <class A>
  inline void reserve_ptr(std::shared_ptr<A> ptr, reserve_context_ptr ref);

  /** Stop preventing ptr from being deleted. */
  template <class A>
  inline void unreserve_ptr(std::shared_ptr<A> ptr, reserve_context_ptr ref);


  /** @cond internal */

  struct ptr_wrapper
  {
    virtual ~ptr_wrapper() {}
  };

  template <class A>
  class ptr_wrapper_of_type : public ptr_wrapper
  {      
    std::shared_ptr<A> ptr;
  public:
    ptr_wrapper_of_type(std::shared_ptr<A> ptr)
      : ptr(ptr) {}
  };

  struct reserve_context
  {  
    std::map<ptr_t, ptr_wrapper*> ptrs;
    // TODO mutex
  public:
    reserve_context() = default;
    ~reserve_context()
    {
      for (std::pair<ptr_t, ptr_wrapper*> ptr : ptrs)
      {
        delete ptr.second;
      }
    }
  };

  reserve_context_ptr create_reserve_context()
  {
    return new reserve_context();
  }
  void destroy_reserve_context(reserve_context_ptr ref)
  {
    delete ref;
  }

  template <class A>
  void reserve_ptr(std::shared_ptr<A> ptr, reserve_context_ptr context)
  {
    if (!context) return;
    ptr_t raw_ptr = (ptr_t) ptr.get();

    if (!context->ptrs[raw_ptr])
      context->ptrs[raw_ptr] = new ptr_wrapper_of_type<A>(ptr);
  }

  template <class A>
  void unreserve_ptr(std::shared_ptr<A> ptr, reserve_context_ptr context)
  {
    if (!context) return;
    ptr_t raw_ptr = (ptr_t) ptr.get();
                          
    delete context->ptrs[raw_ptr];
    context->ptrs[raw_ptr] = nullptr;
  }

  /** @endcond */  

}

