
#pragma once

#include <utility>
#include <list>
#include <functional>
#include <cmath>
#include <scl/utility.hpp>
#include <scl/audio/audio_types.hpp>
#include <scl/audio/processor.hpp>

namespace scl
{
  namespace audio
  {
    namespace processor
    {
      /** @cond internal */

      // (a -> b) -> (a ~> b)
      class raw_unary_processor : public raw_processor
      {
        audio_type in_type;
        audio_type out_type;
      public:
        using function_type = void (*)(ptr_t data, ptr_t input, ptr_t output);
        using deleter_type  = void (*)(ptr_t data);
        using data_type     = ptr_t;
      private:
        function_type function;
        deleter_type deleter;
        data_type data;
      public:
        raw_unary_processor(audio_type    in_type,
                            audio_type    out_type,
                            function_type function,
                            deleter_type  deleter,
                            data_type     data)
          : in_type(in_type)
          , out_type(out_type)
          , function(function)
          , deleter(deleter)
          , data(data) {}

        ~raw_unary_processor()
        {
          if (deleter)
            deleter(data);
        }

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



      template <class A, class B>
      class adapt_pass_by_reference
      {
        using this_type = adapt_pass_by_reference<A,B>;
        std::function<void(const A&, B&)> f;

        inline void call(ptr_t a, ptr_t b)
        {
          const A& aRef = *(const A*)(a);
          B&       bRef = *(B*)(b);
          f(aRef, bRef);
        }
      public:
        adapt_pass_by_reference(std::function<void(const A&, B&)> f): f(f) {}

        static void  function(ptr_t f, ptr_t x, ptr_t y) { ((this_type*) f)->call(x, y); }
        static ptr_t data(std::function<B(A)> f)         { return (ptr_t) new this_type(f); }
        static void  deleter(ptr_t f)                    { delete ((this_type*) f); }
      };

      template <class A, class B>
      class adapt_pass_by_value
      {
        using this_type = adapt_pass_by_value<A,B>;
        std::function<B(A)> f;

        inline void call(ptr_t a, ptr_t b)
        {
          A aVal;
          scl::raw_copy(a, a + sizeof(A), (ptr_t) &aVal);
          B bVal = f(aVal);
          scl::raw_copy((ptr_t) &bVal, (ptr_t) &bVal + sizeof(B), b);
        }
      public:
        adapt_pass_by_value(std::function<B(A)> f) : f(f) {}

        static void  function(ptr_t f, ptr_t x, ptr_t y) { ((this_type*) f)->call(x, y); }
        static ptr_t data(std::function<B(A)> f)         { return (ptr_t) new this_type(f); }
        static void  deleter(ptr_t f)                    { delete ((this_type*) f); }
      };
      

      /** @endcond */

      struct unary_closure
      {
        void (*function) (ptr_t data, ptr_t input, ptr_t output);
        void (*deleter) (ptr_t data);
        ptr_t data;
      };

      /** 
        ## Description
          Applies a unary function.
        
        ## Associated types
          ### State
            `unit`
          ### Argument
            `unit`
          ### Result
            `unit`
          ### Message Input
            `unit`
          ### Message Output
            `unit`
          ### Input
            The argument to the function `A`.
          ### Output
            The result of the function `B`.
       */
      template <class A, class B>
      class unary_processor
        : public processor <
        unit, unit, unit,
        unit, unit,
        A,B >
      {
        raw_processor_ptr raw;
      public:
        unary_processor(std::function<B(A)> f)
          : raw(
            new raw_unary_processor(
              audio_type::get<A>(),
              audio_type::get<B>(),
              adapt_pass_by_value<A,B>::function,
              adapt_pass_by_value<A,B>::deleter,
              adapt_pass_by_value<A,B>::data(f))) {}

        unary_processor(std::function<void(const A&, B&)> f)
          : raw(
            new raw_unary_processor(
              audio_type::get<A>(),
              audio_type::get<B>(),
              adapt_pass_by_reference<A,B>::function,
              adapt_pass_by_reference<A,B>::deleter,
              adapt_pass_by_reference<A,B>::data(f))) {}
        
        unary_processor(unary_closure f)
          : raw(
            new raw_unary_processor(
              audio_type::get<A>(),
              audio_type::get<B>(),
              f.function,
              f.deleter,
              f.data)) {}
        
        raw_processor_ptr get_raw()
        {
          return raw;
        }

        void load(const unit& state)
        {
          raw->load((ptr_t) &state);
        }
        void store(unit& state)
        {
          raw->store((ptr_t) &state);
        }
        void prepare(const unit& argument)
        {
          raw->prepare((ptr_t) &argument);
        }
        void cleanup(unit& result)
        {
          raw->cleanup((ptr_t) &result);
        }
        bool is_ready()
        {
          return raw->is_ready();
        }
        void process(const std::list<unit>& input_messages,
                     const A& input,
                     A& output,
                     std::list<unit>& output_messages)
        {
          raw->process(
            (ptr_t) &input_messages,
            (ptr_t) &input,
            (ptr_t) &output,
            (ptr_t) &output_messages);
        }
      };

      /**
         Lift a unary function to a processor.
       */
      template <class A, class B>
      inline unary_processor<A,B> lift(B(*f)(A))
      {
        return unary_processor<A,B>(std::function<B(A)>(f));
      }
                 
      namespace math32
      {             
        static const unary_processor<sample32,sample32> fabs   = lift<sample32,sample32>(std::fabs);
        static const unary_processor<sample32,sample32> exp    = lift<sample32,sample32>(std::exp);
        static const unary_processor<sample32,sample32> exp2   = lift<sample32,sample32>(std::exp2);
        static const unary_processor<sample32,sample32> expm1  = lift<sample32,sample32>(std::expm1);
        static const unary_processor<sample32,sample32> log    = lift<sample32,sample32>(std::log);
        static const unary_processor<sample32,sample32> log10  = lift<sample32,sample32>(std::log10);
        static const unary_processor<sample32,sample32> log1p  = lift<sample32,sample32>(std::log1p);
        static const unary_processor<sample32,sample32> log2   = lift<sample32,sample32>(std::log2);
        static const unary_processor<sample32,sample32> sqrt   = lift<sample32,sample32>(std::sqrt);
        static const unary_processor<sample32,sample32> cbrt   = lift<sample32,sample32>(std::cbrt);

        static const unary_processor<sample32,sample32> sin    = lift<sample32,sample32>(std::sin);
        static const unary_processor<sample32,sample32> cos    = lift<sample32,sample32>(std::cos);
        static const unary_processor<sample32,sample32> tan    = lift<sample32,sample32>(std::tan);
        static const unary_processor<sample32,sample32> asin   = lift<sample32,sample32>(std::asin);
        static const unary_processor<sample32,sample32> acos   = lift<sample32,sample32>(std::acos);
        static const unary_processor<sample32,sample32> atan   = lift<sample32,sample32>(std::atan);
        static const unary_processor<sample32,sample32> sinh   = lift<sample32,sample32>(std::sinh);
        static const unary_processor<sample32,sample32> cosh   = lift<sample32,sample32>(std::cosh);
        static const unary_processor<sample32,sample32> tanh   = lift<sample32,sample32>(std::tanh);
        static const unary_processor<sample32,sample32> asinh  = lift<sample32,sample32>(std::asinh);
        static const unary_processor<sample32,sample32> acosh  = lift<sample32,sample32>(std::acosh);
        static const unary_processor<sample32,sample32> atanh  = lift<sample32,sample32>(std::atanh);

        static const unary_processor<sample32,sample32> erf    = lift<sample32,sample32>(std::erf);
        static const unary_processor<sample32,sample32> erfc   = lift<sample32,sample32>(std::erfc);
        static const unary_processor<sample32,sample32> lgamma = lift<sample32,sample32>(std::lgamma);
        static const unary_processor<sample32,sample32> tgamma = lift<sample32,sample32>(std::tgamma);

        static const unary_processor<sample32,sample32> ceil   = lift<sample32,sample32>(std::ceil);
        static const unary_processor<sample32,sample32> floor  = lift<sample32,sample32>(std::floor);
        static const unary_processor<sample32,sample32> trunc  = lift<sample32,sample32>(std::trunc);
        static const unary_processor<sample32,sample32> round  = lift<sample32,sample32>(std::round);
      };

      namespace math64
      {             
        static const unary_processor<sample64,sample64> fabs   = lift<sample64,sample64>(std::fabs);
        static const unary_processor<sample64,sample64> exp    = lift<sample64,sample64>(std::exp);
        static const unary_processor<sample64,sample64> exp2   = lift<sample64,sample64>(std::exp2);
        static const unary_processor<sample64,sample64> expm1  = lift<sample64,sample64>(std::expm1);
        static const unary_processor<sample64,sample64> log    = lift<sample64,sample64>(std::log);
        static const unary_processor<sample64,sample64> log10  = lift<sample64,sample64>(std::log10);
        static const unary_processor<sample64,sample64> log1p  = lift<sample64,sample64>(std::log1p);
        static const unary_processor<sample64,sample64> log2   = lift<sample64,sample64>(std::log2);
        static const unary_processor<sample64,sample64> sqrt   = lift<sample64,sample64>(std::sqrt);
        static const unary_processor<sample64,sample64> cbrt   = lift<sample64,sample64>(std::cbrt);

        static const unary_processor<sample64,sample64> sin    = lift<sample64,sample64>(std::sin);
        static const unary_processor<sample64,sample64> cos    = lift<sample64,sample64>(std::cos);
        static const unary_processor<sample64,sample64> tan    = lift<sample64,sample64>(std::tan);
        static const unary_processor<sample64,sample64> asin   = lift<sample64,sample64>(std::asin);
        static const unary_processor<sample64,sample64> acos   = lift<sample64,sample64>(std::acos);
        static const unary_processor<sample64,sample64> atan   = lift<sample64,sample64>(std::atan);
        static const unary_processor<sample64,sample64> sinh   = lift<sample64,sample64>(std::sinh);
        static const unary_processor<sample64,sample64> cosh   = lift<sample64,sample64>(std::cosh);
        static const unary_processor<sample64,sample64> tanh   = lift<sample64,sample64>(std::tanh);
        static const unary_processor<sample64,sample64> asinh  = lift<sample64,sample64>(std::asinh);
        static const unary_processor<sample64,sample64> acosh  = lift<sample64,sample64>(std::acosh);
        static const unary_processor<sample64,sample64> atanh  = lift<sample64,sample64>(std::atanh);

        static const unary_processor<sample64,sample64> erf    = lift<sample64,sample64>(std::erf);
        static const unary_processor<sample64,sample64> erfc   = lift<sample64,sample64>(std::erfc);
        static const unary_processor<sample64,sample64> lgamma = lift<sample64,sample64>(std::lgamma);
        static const unary_processor<sample64,sample64> tgamma = lift<sample64,sample64>(std::tgamma);

        static const unary_processor<sample64,sample64> ceil   = lift<sample64,sample64>(std::ceil);
        static const unary_processor<sample64,sample64> floor  = lift<sample64,sample64>(std::floor);
        static const unary_processor<sample64,sample64> trunc  = lift<sample64,sample64>(std::trunc);
        static const unary_processor<sample64,sample64> round  = lift<sample64,sample64>(std::round);
      };
    }
  }
}

