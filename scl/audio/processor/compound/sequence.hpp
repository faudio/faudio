
#pragma once

#include <algorithm>
#include <utility>
#include <scl/utility.hpp>
#include <scl/audio/processor.hpp>

namespace scl
{
  namespace audio
  {
    namespace processor
    {
      using std::pair;

      // template <class A, class B, class C>
      // class sequence_processor
      //   : public processor < void, void, void,
      //     void, void,
      //     A, C >
      // {
      // public:
      //   sequence_processor(value_type value)
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
      //   }        7
      //   bool is_ready()
      //   {
      //   }
      //   void process(const list<input_message_type>& input_messages,
      //                const input_type& input,
      //                output_type& output,
      //                list<output_message_type>& output_messages)
      //   {
      //   }
      //
      // private:
      // };






      // (a ~> b) -> (b ~> c) -> (a ~> c)
      class raw_sequence_processor : public raw_processor
      {
        raw_processor_ptr f, g;
      public:
        using parent_type = raw_processor;

        raw_sequence_processor(raw_processor_ptr f,
                               raw_processor_ptr g)
          : raw_processor(0, 0, 0,
                          f->input_size,
                          g->output_size)
          , f(f)
          , g(g) 
        { 
          assert(g->input_size == 0 || f->output_size == g->input_size); 
        }

        void load(intptr_t state) {}
        void store(intptr_t state) {}

        void prepare(intptr_t arg)
        {
          f->prepare(arg);
          g->prepare(arg);
        }

        void cleanup(intptr_t res)
        {
          f->cleanup(res);
          g->cleanup(res);
        }

        bool is_ready()
        {
          return f->is_ready() && g->is_ready();
        }

        void process(intptr_t in_msg, intptr_t input, intptr_t output, intptr_t out_msg)
        {
          size_t size = parent_type::input_size;
          if (f->input_size < g->output_size) // compare sizes
          {
            f->process(in_msg, input, output, out_msg);
            g->process(in_msg, output, input, out_msg);
            scl::raw_copy(input, input + size, output);
          }
          else
          {
            scl::raw_copy(input, input + size, output);
            f->process(in_msg, output, input, out_msg);
            g->process(in_msg, input, output, out_msg);
          }
        }
      };
    }
  }
}

