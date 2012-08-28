
#pragma once

namespace scl
{
  namespace audio
  {
    namespace scheduler
    {
      template <class Task, class Time = time>
      class unlocked_scheduler
      {
      public:
        using task_type = Task;
        using time_type = Time;
      private:
        using queue_base_type = std::vector<task_type>;
        using queue_type = std::priority_queue<task_type, queue_base_type, not_less_than>
                         public:
                           unlocked_scheduler(const improving<time_type>& time)
                             : time(time) {}

        template <TimeTypeRange>
        future<value_type> schedule(TimeTypeRange times, task_type && task)
        {
          int times = std::length(times);
        }

        void perform()
        {
        }

      private:
        improving<time_type> time;
        queue_type queue;
      };
    }
  }
}