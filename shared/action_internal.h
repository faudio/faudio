
#include <fa/action.h>

void run_actions(fa_priority_queue_t controls, fa_time_t now, fa_binary_t function, fa_ptr_t data);
void run_and_resched_action(fa_action_t action, fa_time_t time, fa_time_t now, fa_list_t* resched, fa_binary_t function, fa_ptr_t data);


