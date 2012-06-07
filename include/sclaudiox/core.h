/*
    ScoreCleaner Audio Engine
    
    Copyright (c) 2012 DoReMIR Music Research AB.
    All rights reserved.
 */

#ifndef  _SCLAUDIOX_CORE
#define  _SCLAUDIOX_CORE

#include <boost/smart_ptr/intrusive_ptr.hpp>
#include <boost/utility.hpp>

#include "sclaudiox/defines.h"
#include "sclaudiox/numeric.h"
#include "sclaudiox/time.h"
#include "sclaudiox/util/string.h"
#include "sclaudiox/util/misc.h"


/** 
    \ingroup sclaudiox
  */
namespace doremir {
namespace scl {

    /**
        An audio sample.
     */
    typedef float Sample;  

    class SCLAUDIO_API FileHandle;

    class SCLAUDIO_API AudioBuffer;

    /**
        A simple data structure used to pass information to a call.

        Invariants:

          - Each subclass has non-throwing, no-argument contructor. Other constuctors are allowed.
          - Each subclass is copyable and assignable
          - Each subclass no methods or custom destructors.
     */
    class SCLAUDIO_API Options {};
    
    class SCLAUDIO_API NonCopyable : public boost::noncopyable {};

    /**
        A mangaged resource.
     */
    class SCLAUDIO_API Resource
    {
    public:
        Resource() : referenceCount(0) {}
        virtual ~Resource() {}

    private:
        template <typename T> friend T*   acquire(T* obj);
        template <typename T> friend void release(T* obj);
        int referenceCount;
    };


    /**
        Acquires a reference to the given object.
     */
    template <class Managed>
    Managed* acquire(Managed* obj)
    {
        if (obj != NULL) ++(obj->referenceCount);
        return obj;
    }

    /**
        Releases a reference to the given object, deleting it iff there are
        no other object is referencing it.
     */
    template <class Managed>
    void release(Managed* obj)
    {
        if (obj != NULL && --(obj->referenceCount) == 0) delete (Resource*) obj;
    }

    template <class Managed>
    Managed acquireAll (Managed first, Managed last)
    {
        while (first != last) acquire(*first++); return first;
    }

    template <class Managed>
    void releaseAll (Managed first, Managed last)
    {
        while (first != last) release(*first++);
    }

    template <class Managed>
    Managed acquireAll(Managed seq)
    {
        acquireAll(seq.begin(), seq.end()); return seq;
    }

    template <class Managed>
    void releaseAll (Managed seq)
    {
        releaseAll(seq.begin(), seq.end());
    }          


} // namespace scl
} // namespace doremir


/// \cond internal
namespace boost
{
    template <class T>
    void intrusive_ptr_add_ref(T * p)
    {
        acquire(p);
    }
    template <class T>
    void intrusive_ptr_release(T * p)
    {
        release(p);    
    }
}  
/// \endcond internal



#endif

