
#include <fa/fa.h>
#include <fa/util.h>
#include <fa/dynamic.h>
#include <fa/string.h>
#include <fa/func_ref.h>
#include "common.h"

#include <stdio.h>
#include <sys/types.h> 
#include <sys/socket.h>
#include <netinet/in.h>
#include <unistd.h>

/*
Erik's test file
*/


int note_count = 0;

bool note_pred(fa_ptr_t env, fa_ptr_t data) {
    fa_slog_info(" note_pred: ", env, data);
    return true; //(note_count++ % 2) == 0;
}

bool five_times(fa_ptr_t env, fa_ptr_t data) {
    static int x = 0;
    fa_slog_info(" ### five_times: ", fa_from_int16(x));
    return x++ > 2;
}

void run_dls()
{
#ifndef _WIN32
	fa_string_t name   = fa_string("dls");
	fa_pair_t synth = fa_signal_dls(name);
#else
	fa_string_t name   = fa_string("fluid");
	fa_pair_t synth = fa_signal_synth(name, fa_string("C:\\sf.sf2"));
#endif
    
    
	fa_audio_session_t s = fa_audio_begin_session();
	//fa_audio_device_t i  = fa_audio_default_input(s);
	fa_audio_device_t o  = fa_audio_default_output(s);

	//fa_mark_used(synth);

    //fa_list_t out        = fa_empty();	

	fa_list_t out           = fa_pair_to_list(synth);

	fa_audio_stream_t st = fa_audio_open_stream(NULL, o, just, out);

	if (fa_check(st)) {
		fa_error_log(st, NULL);
	}

    
    fa_thread_sleep(1000);
    
    fa_log_region_count("BEGIN");
    fa_log_list_count();
    fa_log_time_count();
    fa_log_pair_count();
    fa_log_pair_left_count();
    fa_log_string_count();
    fa_log_func_ref_count();
    
    fa_thread_sleep(500);

    // fa_action_t action0 = fa_action_send(name, fa_midi_message_create_simple(0x90, 64, 90));
    // fa_log_region_count();
    // fa_deep_destroy(action0);
    // fa_log_region_count();
    //
    // fa_thread_sleep(500);
    
    fa_action_t action1 = fa_action_if(note_pred, NULL, fa_action_send(name, fa_midi_message_create_simple(0x90, 64, 90)));
    fa_action_t action2 = fa_action_if(note_pred, NULL, fa_action_send(name, fa_midi_message_create_simple(0x90, 66, 90)));
    fa_action_t action3 = fa_action_if(note_pred, NULL, fa_action_send(name, fa_midi_message_create_simple(0x90, 68, 90)));
    fa_action_t action4 = fa_action_if(note_pred, NULL, fa_action_send(name, fa_midi_message_create_simple(0x90, 69, 90)));
    //fa_action_t action1 = fa_action_send(name, fa_midi_message_create_simple(0x90, 68, 90));
    //fa_action_t action2 = fa_action_send(name, fa_midi_message_create_simple(0x90, 67, 90));
    //fa_action_t many_action = fa_action_if(note_pred, NULL, fa_action_many(list(pair(action1, fa_milliseconds(500)), pair(action2, fa_milliseconds(500)))));
    fa_action_t many_action = fa_action_many(list(
        pair(action1, fa_milliseconds(100)),
        pair(action2, fa_milliseconds(100)),
        pair(action3, fa_milliseconds(100)),
        pair(action4, fa_milliseconds(100))));
    fa_action_t repeat_action = fa_action_repeat(fa_milliseconds(1000), many_action);
    fa_action_t while_action = fa_action_until(five_times, NULL, repeat_action);
    //fa_action_t many_many_action = fa_action_many(list(pair(many_action, fa_milliseconds(20))));
    
    //fa_action_t empty_many_action = fa_action_many(fa_list_empty());
        
    //fa_action_t while_action = fa_action_while(note_pred, NULL, many_action);
    
        
    //print_all_actions();
    
    fa_log_region_count("Before scheduling");
    fa_log_list_count();
    fa_log_time_count();
    fa_log_pair_count();
    fa_log_pair_left_count();
    fa_log_string_count();
    fa_log_func_ref_count();
    fa_log_action_count();

    fa_audio_schedule_relative(fa_hms(0, 0, 0), while_action, st);
    
    //fa_deep_destroy_always(many_many_action);
            
        //         fa_hms(0, 0, 0)
        //             )

    // for (int x = 0; x < 3; ++x) {
    //     for (int i = 0; i < 10; ++i) {
    //
    //         // fa_clock_t cl = fa_clock_standard();
    //         // fa_clock_t cl = fa_audio_stream_clock(st);
    //         // fa_mark_used(cl);
    //
    //         // printf("Scheduling msec: %lld \n", fa_clock_milliseconds(cl));
    //         // printf("Scheduling time: %s \n", unstring(fa_string_show(fa_clock_time(cl))));
    //
    //         fa_action_t chord = fa_action_many(list(
    //             fa_pair_create(
    //                 fa_action_send(name, fa_midi_message_create_simple(0x90, 64 + ((i % 12) * 3), 90)),
    //         fa_hms(0, 0, 0)
    //             ),
    //         fa_pair_create(
    //             fa_action_send(name, fa_midi_message_create_simple(0x90, 60 + ((i % 12) * 3), 90)),
    //         fa_hms(0, 0, 0)
    //             )
    //                 ));
    //         // printf("System time (early): %lld\n", fa_clock_milliseconds(fa_clock_standard()));
    //         fa_audio_schedule_relative(
    //             fa_hms(0, 0, 0),
    //         chord,
    //         st);
    //         fa_log_region_count();
    //         fa_thread_sleep(150);
    //         fa_log_region_count();
    //     }
    //     fa_log_region_count();
    // }
    fa_log_region_count("Before sleeping");
    fa_slog_info("Now sleeping...");
    fa_thread_sleep(8000);
    fa_log_region_count("After sleeping");
    fa_log_list_count();
    fa_log_time_count();
    fa_log_pair_count();
    fa_log_pair_left_count();
    fa_log_string_count();
    fa_log_func_ref_count();
    fa_log_action_count();
    //print_all_actions();
	
    fa_destroy(name);
    fa_destroy(synth);

	fa_destroy(st);
	fa_destroy(s);
	
}

bool DESTROY_NEVER(fa_ptr_t ptr)
{
    return false;
}

int main(int argc, char const *argv[])
{
	//fa_set_log_tool();
  fa_set_log_std();
  
  
  // fa_log_region_count("before");
  // fa_list_t blist = list(fa_from_int8(1), fa_from_int8(2), fa_from_int8(3));
  // fa_log_region_count("one");
  // fa_list_t clist = fa_list_tail(blist);
  // fa_list_t dlist = fa_list_tail(clist);
  // fa_log_region_count("two");
  // fa_destroy(blist);
  // fa_log_region_count("three");
  // fa_destroy(dlist);
  // fa_log_region_count("four");
  // fa_destroy(clist);
  // fa_log_region_count("after");
  // return 0;
  
  // fa_map_t map = fa_map_empty();
  // fa_slog_info("map 1: ", map);
  // fa_map_dadd(fa_string("a"), fa_string("b"), map);
  // fa_slog_info("map 2: ", map);
  // fa_destroy(map);
  //
  
    
  fa_with_faudio() {
      run_dls();
  }

  return 0;
  
  fa_log_region_count("begin 1");
  fa_string_t aname = fa_string("ape");
  fa_action_t a1 = fa_action_send(aname, fa_midi_message_create_simple(1, 2, 3));
  fa_action_t a2 = fa_action_send(aname, fa_midi_message_create_simple(5, 7, 8));
  fa_action_t a3 = fa_action_send(aname, fa_midi_message_create_simple(11, 15, 67));
  fa_destroy(aname);
  fa_list_t alist = list(fa_pair_create(a1, fa_milliseconds(100)),
  fa_pair_create(a2, fa_milliseconds(200)),
  fa_pair_create(a3, fa_milliseconds(300))
       );
  fa_action_t many = fa_action_many(alist);
  
  fa_log_region_count("middle");
  
  
  fa_deep_destroy(many, DESTROY_ALWAYS);
  
  fa_log_region_count("after destroying many");
  
	
  fa_log_region_count("Begin:");
  
  fa_string_t hej = fa_string("hej");
  fa_log_region_count("");
  char* chej = fa_unstring(hej);
  fa_log_region_count("");
  fa_free(chej);
  fa_log_region_count("");
  fa_deep_destroy_always(hej);

  fa_log_region_count("After strings:");
  
  fa_list_t list = list();
  printf("Has a list\n");
  fa_print("list  ==> %s\n", list);
  fa_log_region_count("");
  list = fa_list_dcons(fa_i32(1), list);
  list = fa_list_dcons(fa_i32(2), list);
  fa_print("list  ==> %s\n", list);
  fa_log_region_count("");
  fa_deep_destroy_always(list);
  fa_log_region_count("");
  
  
	fa_string_t json_string = fa_string("[1, 2, 3]");
	fa_log_info(json_string);
  
    fa_log_region_count("");
	
	//JSON_Value *json = json_parse_string(fa_unstring(string));
	
    fa_ptr_t data = fa_string_from_json(json_string); // misnomer, it's not a string that is created but an object
    fa_print("data  ==> %s\n", data);
    //fa_print("head  ==> %s\n", fa_list_head(data));
    
    fa_log_region_count("");
	
    switch (fa_dynamic_get_type(data)) {
    case pair_type_repr:
		printf("it's a pair!\n");
		break;

    case set_type_repr:
		printf("it's a set!\n");
		break;

    case list_type_repr:
		printf("it's a list!\n");
		break;

    case map_type_repr:
		printf("it's a map!\n");
		break;
		
    default:
        printf("it's something else!\n");
    }
    
    fa_log_region_count("");
    fa_dlog_info(fa_string("destroying data"));
    fa_deep_destroy_always(data);
    
    fa_log_region_count("");
    fa_dlog_info(fa_string("destroying json_string"));
    fa_deep_destroy_always(json_string);
	
	
	// for (int i = 0; i < 5; ++i) {
	// 	for (int j = 0; j < 10000; ++j) {
	// 		//fa_action_t action = fa_action_send(fa_string("ape"), fa_string("nisse"));
	// 		//fa_action_t action = fa_action_null();
	// 		fa_action_t action = fa_action_repeat(fa_seconds(1), fa_action_null());
	// 		fa_destroy(action);
	//
	// 		//fa_time_t time_value = fa_seconds(0);
	// 	}
	// 	fa_thread_sleep(1500);
	// }
	
	// fa_with_faudio() {
	// 	run_dls();
	// 	// for (int i = 0; i < 5; ++i) {
	// 		//run_dls();
	// 	// }
	// 	fa_log_region_count();
	// 	//fa_dlog_warning(fa_string("This is a warning"));
	// 	fa_log_region_count();
	// 	//fa_log_region_count();
	// 	//fa_log_region_count();
	// 	fa_log_region_count();
	// }
	
	// fa_log_region_count();
	// fa_ratio_t ratio = fa_ratio_create(2, 5);
	// fa_log_region_count();
	// fa_dlog_info(fa_string_dshow(ratio));
	// fa_log_region_count();
	
	fa_log_region_count("");
	fa_string_t str = fa_string("nisse");
	fa_print("ape 1 %s", str);
	fa_log_region_count("");
	fa_print("ape 2 %s", str);
	fa_deep_destroy_always(str);
	fa_log_region_count("");
	
    fa_dlog_info(fa_string("-------------------"));
    fa_log_region_count("");
	//fa_log_region_count();
	//fa_dlog_info(fa_string("ape"));
	fa_midi_message_t msg1 = fa_midi_message_create_simple(5, 7, 8);
	//fa_midi_message_t msg2 = fa_midi_message_create_simple(7, 8, 9);
	//fa_midi_message_t msg3 = fa_midi_message_create_simple(7, 8, 10);
  fa_dlog_info(fa_string("Before creating action"));
  fa_log_region_count("");
  fa_string_t action_name = fa_string("ape");
  fa_action_t action1 = fa_action_send(action_name, msg1);
  fa_destroy(action_name);
  fa_dlog_info(fa_string("After creating action"));
  fa_log_region_count("");
	// fa_destroy(msg1);
	// fa_destroy(msg2);
	// fa_destroy(msg3);
	//fa_log_region_count();
	//fa_dlog_info(fa_string_dshow(msg1));
	//fa_log_region_count();
	//fa_destroy(msg1);
	//fa_log_region_count();
  fa_dlog_info(fa_string("Before destroying action"));
  fa_log_region_count("");
  fa_deep_destroy_always(action1);
  fa_dlog_info(fa_string("After destroying action"));
  fa_log_region_count("");
	//fa_deep_destroy_always(msg2);
	//fa_deep_destroy_always(msg3);
  //fa_dlog_info(fa_string("After destroying messages"));
	//fa_log_region_count();
}
