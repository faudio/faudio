
#pragma once

#include <algorithm>
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
      using std::pair;

      // template <class A, class B>
      // class constant_processor
      //   : public processor < void, void, void,
      //     void, void,
      //     A, B >
      // {
      // public:
      //   constant_processor(output_type value)
      //     : value(value) {}
      //
      //   void prepare(const argument_type& argument)
      //   {
      //   }
      //   void cleanup(result_type& result)
      //   {
      //   }
      //   void load(const state_type& state)
      //   {
      //   }
      //   void store(state_type& state)
      //   {
      //   }
      //   bool is_ready()
      //   {
      //     return value ? true : false;
      //   }
      //   void process(const list<input_message_type>& input_messages,
      //                const input_type& input,
      //                output_type& output,
      //                list<output_message_type>& output_messages)
      //   {
      //     output = value;
      //   }
      //
      // private:
      //   output_type value;
      // };





      // (b ~> a)
      class raw_constant_processor : public raw_processor
      {
        std::unique_ptr<char> buffer;
      public:
        raw_constant_processor(size_t size)
          : raw_processor(0, 0, 0,
                          0, size) {}

        void load(intptr_t state) {}
        void store(intptr_t state) {}

        void prepare(intptr_t arg)
        {
          size_t size = raw_processor::output_size;
          buffer.reset(new char[size]);
          intptr_t buf = (intptr_t)(buffer.get());
          scl::raw_copy(arg, arg + size, buf);
        }

        void cleanup(intptr_t res)
        {
          buffer.reset();
        }

        bool is_ready()
        {
          return true;
        }

        void process(intptr_t in_msg,
                     intptr_t input,
                     intptr_t output,
                     intptr_t out_msg)
        {
          size_t size = raw_processor::output_size;
          intptr_t buf = (intptr_t)(buffer.get());
          scl::raw_copy(buf, buf + size, output);
        }
      };

    }
  }
}

