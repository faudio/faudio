
#pragma once

#include <utility>
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

      // (a -> b -> c) -> ((a,b) ~> c)
      class raw_binary_processor : public raw_processor
      {
        audio_type fst_in_type;
        audio_type snd_in_type;
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
        raw_binary_processor(audio_type    fst_in_type,
                             audio_type    snd_in_type,
                             audio_type    out_type,
                             function_type function,
                             deleter_type  deleter,
                             data_type     data)
          : fst_in_type(fst_in_type)
          , snd_in_type(snd_in_type)
          , out_type(out_type)
          , function(function)
          , deleter(deleter)
          , data(data) {}

        ~raw_binary_processor()
        {
          if (deleter)
            deleter(data);
        }

        audio_type input_type()
        {
          return audio_type::pair(fst_in_type, snd_in_type);
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
      class adapt_pass_by_reference2
      {
        using this_type = adapt_pass_by_reference2<A,B>;
        std::function<void(const A&, B&)> f;

        inline void call(ptr_t a, ptr_t b)
        {
          // TODO
        }
      public:
        adapt_pass_by_reference(std::function<void(const A&, B&)> f): f(f) {}

        static void  function(ptr_t f, ptr_t x, ptr_t y) { ((this_type*) f)->call(x, y); }
        static ptr_t data(std::function<B(A)> f)         { return (ptr_t) new this_type(f); }
        static void  deleter(ptr_t f)                    { delete ((this_type*) f); }
      };

      template <class A, class B>
      class adapt_pass_by_value2
      {
        using this_type = adapt_pass_by_value2<A,B>;
        std::function<B(A)> f;

        inline void call(ptr_t a, ptr_t b)
        {
          // TODO
        }
      public:
        adapt_pass_by_value(std::function<B(A)> f) : f(f) {}

        static void  function(ptr_t f, ptr_t x, ptr_t y) { ((this_type*) f)->call(x, y); }
        static ptr_t data(std::function<B(A)> f)         { return (ptr_t) new this_type(f); }
        static void  deleter(ptr_t f)                    { delete ((this_type*) f); }
      };
      

      /** @endcond */

      struct binary_closure
      {
        void (*function) (ptr_t data, ptr_t input, ptr_t output);
        void (*deleter) (ptr_t data);
        ptr_t data;
      };

      /** 
        ## Description
          Applies a binary function.
        
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
            The pair of arguments to the function `(A,B)`.
          ### Output
            The result of the function `C`.
       */
      template <class A, class B, class C>
      class binary_processor
        : public processor <
        unit, unit, unit,
        unit, unit,
        typename audio_pair<A,B>::type, 
        B >
      {
        raw_processor_ptr raw;
      public:
        binary_processor(std::function<C(A,B)> f)
          : raw(
            new raw_binary_processor(
              audio_type::get<A>(),
              audio_type::get<B>(),
              audio_type::get<C>(),
              adapt_pass_by_value2<A,B,C>::function,
              adapt_pass_by_value2<A,B,C>::deleter,
              adapt_pass_by_value2<A,B,C>::data(f))) {}

        binary_processor(std::function<void(const A&, const B&, C&)> f)
          : raw(
            new raw_binary_processor(
              audio_type::get<A>(),
              audio_type::get<B>(),
              audio_type::get<C>(),
              adapt_pass_by_reference2<A,B,C>::function,
              adapt_pass_by_reference2<A,B,C>::deleter,
              adapt_pass_by_reference2<A,B,C>::data(f))) {}
        
        binary_processor(binary_closure f)
          : raw(
            new raw_binary_processor(
              audio_type::get<A>(),
              audio_type::get<B>(),
              audio_type::get<C>(),
              f.function,
              f.deleter,
              f.data)) {}

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

      template <class A, class B, class C>
      inline binary_processor<A,B,C> lift2(C(*f)(A,B))
      {
        return binary_processor<A,B,C>(std::function<C(A,B)>(f));
      }
    }
  }
}

