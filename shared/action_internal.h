
#include <fa/action.h>

void run_actions(priority_queue_t controls, fa_time_t now, fa_unary_t function, fa_ptr_t data);
void run_and_resched_action(fa_action_t action, time_t time, time_t now, list_t* resched, fa_unary_t function, fa_ptr_t data);


