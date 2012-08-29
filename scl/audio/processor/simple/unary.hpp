
#pragma once

#include <utility>
#include <list>
#include <functional>
#include <scl/utility.hpp>
#include <scl/audio/audio_types.hpp>
#include <scl/audio/processor.hpp>

namespace scl
{
  namespace audio
  {
    namespace processor
    {
      // (a -> b) -> (a ~> b)
      class raw_unary_processor : public raw_processor
      {
        audio_type in_type;
        audio_type out_type;
        
        void (*function) (ptr_t data, ptr_t input, ptr_t output);
        ptr_t data;

      public:
        using function_type = void (*) (ptr_t data, ptr_t input, ptr_t output);

        raw_unary_processor(audio_type in_type,
                            audio_type out_type,
                            function_type function,
                            ptr_t data)
          : in_type(in_type)
          , out_type(out_type)
          , function(function) {}

        audio_type input_type()
        {
          return in_type;
        }
        audio_type output_type()
        {
          return out_type;
        }

        void prepare(ptr_t argument) {}
        void cleanup(ptr_t argument) {}
        void load(ptr_t argument) {}
        void store(ptr_t argument) {}
        bool is_ready()
        {
          return true;
        }

        void process(ptr_t input_messages,
                     ptr_t input,
                     ptr_t output,
                     ptr_t output_messages)
        {
          function(data, input, output);
        }
      };





      struct dynamic_unary
      {
        virtual void call(ptr_t in, ptr_t out) = 0;
      };                          

      // FIXME generate suitable converters for all audio types
      // FIXME pass by reference? how?
      template <class A, class B>
      struct dynamic_unary_wrapper : public dynamic_unary
      {                           
        std::function<B(A)> f;
        dynamic_unary_wrapper(std::function<B(A)> f) : f(f) {}

        void call(ptr_t in, ptr_t out)
        {
          A a;
          B b;
          scl::raw_copy(in, in + sizeof(A), (ptr_t) &a);
          b = f(a);
          scl::raw_copy((ptr_t)&b, (ptr_t)&b + sizeof(B), out);
        } 
      };

      template <class A, class B>
      class unary_processor
        : public processor < 
          unit, unit, unit,
          unit, unit,
          A, B >
      {
        std::function<B(A)> function;
        raw_processor_ptr raw;
      public:
        unary_processor(std::function<B(A)> function)
          : function(function)
          , raw(
              new raw_unary_processor(
                audio_type::get<A>(),
                audio_type::get<B>(),
                NULL,
                NULL)
          ) {}
      };

    }
  }
}

