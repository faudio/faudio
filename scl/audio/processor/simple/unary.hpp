
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
      class unary_ref_closure
      {
        using this_type = unary_ref_closure<A, B>;
        std::function<void(const A&, B&)> f;
      public:
        unary_ref_closure(std::function<void(const A&, B&)> f)
          : f(f) {}

        inline void call(ptr_t x, ptr_t y)
        {
          const A& a = *(const A*)(x);
          B&       b = *(B*)(y);
          f(a, b);
        }

        static void caller(ptr_t f, ptr_t x, ptr_t y)
        {
          ((this_type*) f)->call(x, y);
        }
        static void deleter(ptr_t f)
        {
          delete((this_type*) f);
        }
        static ptr_t data(std::function<void(const A&, B&)> f)
        {
          return (ptr_t) new this_type(f);
        }
      };

      template <class A, class B>
      class unary_val_closure
      {
        using this_type = unary_val_closure<A, B>;
        std::function<B(A)> f;
      public:
        unary_val_closure(std::function<B(A)> f)
          : f(f) {}

        inline void call(ptr_t x, ptr_t y)
        {
          A a;
          scl::raw_copy(x, x + sizeof(A), (ptr_t) &a);
          B b = f(a);
          scl::raw_copy((ptr_t)&b, (ptr_t)&b + sizeof(B), y);
        }

        static void caller(ptr_t f, ptr_t x, ptr_t y)
        {
          ((this_type*) f)->call(x, y);
        }
        static void deleter(ptr_t f)
        {
          delete((this_type*) f);
        }
        static ptr_t data(std::function<B(A)> f)
        {
          return (ptr_t) new this_type(f);
        }
      };


      /** @endcond internal */

      template <class A, class B>
      class unary_processor
        : public processor <
        unit, unit, unit,
        unit, unit,
        A, B >
      {
        raw_processor_ptr raw;
      public:
        unary_processor(std::function<B(A)> f)
          : raw(
            new raw_unary_processor(
              audio_type::get<A>(),
              audio_type::get<B>(),
              unary_val_closure<A, B>::caller,
              unary_val_closure<A, B>::deleter,
              unary_val_closure<A, B>::data(f))) {}

        unary_processor(std::function<void(const A&, B&)> f)
          : raw(
            new raw_unary_processor(
              audio_type::get<A>(),
              audio_type::get<B>(),
              unary_ref_closure<A, B>::caller,
              unary_ref_closure<A, B>::deleter,
              unary_ref_closure<A, B>::data(f))) {}

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

      template <class A, class B>
      inline unary_processor<A, B> lift(B(*f)(A))
      {
        return unary_processor<A, B>(f);
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
    }
  }
}

