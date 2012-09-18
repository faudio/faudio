
#pragma once

#include <map>
#include <memory>

#include <scl/thread.hpp>
// #include <scl/exception.hpp>

namespace scl 
{
  struct reserve_context;
  typedef reserve_context* reserve_context_ptr;

  inline reserve_context_ptr create_reserve_context();
  inline void destroy_reserve_context(reserve_context_ptr context);

  /** 
      Prevent the given shared_ptr from being deleted as long as the given context exists. 
    */
  template <class A>
  inline void reserve_ptr(std::shared_ptr<A> ptr, reserve_context_ptr context);

  /** 
      Stop preventing the given shared_ptr from being deleted.
    */
  template <class A>
  inline void unreserve_ptr(std::shared_ptr<A> ptr, reserve_context_ptr context);

  /** 
      Access the referenced shared_ptr, if one exists. Otherwise, return an empty shared_ptr. 
   */
  template <class A>
  inline std::shared_ptr<A> get_reserved_ptr(A* ptr, reserve_context_ptr context);

  /** @cond internal */

  struct ptr_wrapper
  {
    virtual ~ptr_wrapper() {}
  };

  template <class A>
  class ptr_wrapper_of_type : public ptr_wrapper
  {      
  public:
    std::shared_ptr<A> ptr;
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
  void destroy_reserve_context(reserve_context_ptr context)
  {
    delete context;
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

  template <class A>
  inline std::shared_ptr<A> get_reserved_ptr(A* ptr, reserve_context_ptr context)
  {      
    ptr_t raw_ptr = (ptr_t) ptr;
    
    if (!context || !context->ptrs[raw_ptr])
      return std::shared_ptr<A>();
    
    ptr_wrapper_of_type<A>* wrapped_ptr = (ptr_wrapper_of_type<A>*) context->ptrs[raw_ptr];
    return wrapped_ptr->ptr;
  }

  /** @endcond */  

}

