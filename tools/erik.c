
#include <fa/fa.h>
#include <fa/util.h>
#include <fa/dynamic.h>
#include <fa/string.h>
#include <fa/func_ref.h>
#include "common.h"

#include <stdio.h>
#include <sys/types.h> 
// #include <sys/socket.h>
// #include <netinet/in.h>
#include <unistd.h>

/*
Erik's test file
*/


void fa_clock_initialize();

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
    
    fa_clock_initialize();
    
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
    fa_list_log_count();
    fa_time_log_count();
    fa_pair_log_count();
    fa_pair_left_log_count();
    fa_string_log_count();
    fa_func_ref_log_count();
    
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
    fa_action_t repeat_action = fa_action_repeat(fa_milliseconds(1000), 0, many_action);
    fa_action_t while_action = fa_action_until(five_times, NULL, repeat_action);
    //fa_action_t many_many_action = fa_action_many(list(pair(many_action, fa_milliseconds(20))));
    
    //fa_action_t empty_many_action = fa_action_many(fa_list_empty());
        
    //fa_action_t while_action = fa_action_while(note_pred, NULL, many_action);
    
        
    //print_all_actions();
    
    fa_log_region_count("Before scheduling");
    fa_list_log_count();
    fa_time_log_count();
    fa_pair_log_count();
    fa_pair_left_log_count();
    fa_string_log_count();
    fa_func_ref_log_count();
    fa_action_log_count();

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
    fa_thread_sleep(6000);
    fa_log_region_count("After sleeping");
    fa_list_log_count();
    fa_time_log_count();
    fa_pair_log_count();
    fa_pair_left_log_count();
    fa_string_log_count();
    fa_func_ref_log_count();
    fa_action_log_count();
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

bool _action_sort(fa_ptr_t a, fa_ptr_t b)
{
    return fa_less_than(fa_pair_second(a), fa_pair_second(b));
}

int main(int argc, char const *argv[])
{
	//fa_set_log_tool();
  fa_set_log_std();
  
  
  // // Map test
  // fa_log_region_count(fa_string(" 0: "));
  // fa_map_t map = fa_map_empty();
  // fa_map_set_value_destructor(map, fa_destroy);
  // fa_log_region_count(fa_string(" 1: "));
  // map = fa_map_dadd(fa_string("ape1"), fa_string("nisse"), map);
  // fa_slog_info("map is now: ", map);
  // fa_log_region_count(fa_string(" 2: "));
  // map = fa_map_dset(fa_string("ape2"), fa_string("kalle"), map);
  // map = fa_map_dset(fa_i16(43), fa_string("hopp"), map);
  // map = fa_map_dset(fa_i32(32), fa_string("tomte"), map);
  // fa_log_region_count(fa_string(" 5: "));
  // fa_slog_info("map is now: ", map);
  // map = fa_map_dremove(fa_i16(43), map);
  // fa_slog_info("after removal: ", map);
  // fa_ptr_t val = fa_map_dget(fa_string("ape"), map);
  // fa_log_region_count(fa_string(" 8: "));
  // fa_slog_info("Got val: ", val);
  //
  // fa_log_region_count(fa_string("10: "));
  //
  // //fa_deep_destroy_always(map);
  // fa_destroy(map);
  // fa_log_region_count(fa_string("15: "));
  //
  // bool a_bool = false;
  //
  // fa_map_t map1 = map(fa_i16(1), &a_bool, fa_i32(2), NULL);
  // //fa_map_t map2 = map(fa_i16(2), NULL, fa_i16(1), &a_bool);
  //
  // bool* bool_ref = fa_map_dget(fa_from_float(1.0), map1);
  // printf("&a_bool = %p  bool_ref = %p\n", &a_bool, bool_ref);
  // fa_slog_info("1 is: ", fa_from_bool(*bool_ref));
  // a_bool = true;
  // //bool_ref = fa_map_dget(fa_i16(1), map1);
  // fa_slog_info("1 is: ", fa_from_bool(*bool_ref));
  //
  // fa_destroy(map1);
  //
  // fa_log_region_count(fa_string("20: "));
  //
  // //fa_slog_info("map1: ", map1);
  // //fa_slog_info("map2: ", map2);
  // //fa_slog_info("equal: ", fa_from_bool(fa_equal(map1, map2)));
  //
  // return 0;
  
  
  // fa_string_t name = fa_string("str");
  // fa_action_t ac1 = fa_action_if(note_pred, NULL, fa_action_send(name, fa_midi_message_create_simple(0x90, 64, 90)));
  // fa_action_t ac2 = fa_action_if(note_pred, NULL, fa_action_send(name, fa_midi_message_create_simple(0x90, 66, 90)));
  // fa_action_t ac3 = fa_action_if(note_pred, NULL, fa_action_send(name, fa_midi_message_create_simple(0x90, 68, 90)));
  // fa_action_t ac4 = fa_action_if(note_pred, NULL, fa_action_send(name, fa_midi_message_create_simple(0x90, 69, 90)));
  // //fa_action_t action1 = fa_action_send(name, fa_midi_message_create_simple(0x90, 68, 90));
  // //fa_action_t action2 = fa_action_send(name, fa_midi_message_create_simple(0x90, 67, 90));
  // //fa_action_t many_action = fa_action_if(note_pred, NULL, fa_action_many(list(pair(action1, fa_milliseconds(500)), pair(action2, fa_milliseconds(500)))));
  // fa_list_t actions = list(
  //   pair(ac1, fa_milliseconds(200)),
  //   pair(ac2, fa_milliseconds(100)),
  //   pair(ac3, fa_milliseconds(400)),
  //   pair(ac4, fa_milliseconds(300)));
  //
  //   fa_destroy(name);
  //
  // fa_log_region_count("Before sort");
  // fa_log_list_count();
  //
  // actions = fa_list_dsort(actions, _action_sort);
  //
  // fa_log_region_count("After sort");
  // fa_log_list_count();
  //
  // return 0;
  
  
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
  
  
  fa_deep_destroy_always(many);
  
  fa_log_region_count("after destroying many");
  
	
  fa_log_region_count("Begin:");
  
  fa_string_t hej = fa_string("hej");
  fa_log_region_count(NULL);
  char* chej = fa_unstring(hej);
  fa_log_region_count(NULL);
  fa_free(chej);
  fa_log_region_count(NULL);
  fa_deep_destroy_always(hej);

  fa_log_region_count("After strings:");

  fa_list_t list = fa_list_empty();
  printf("Has a list\n");
  fa_print("list  ==> %s\n", list);
  fa_log_region_count(NULL);
  list = fa_list_dcons(fa_i32(1), list);
  list = fa_list_dcons(fa_i32(2), list);
  fa_print("list  ==> %s\n", list);
  fa_log_region_count(NULL);
  fa_deep_destroy_always(list);
  fa_log_region_count(NULL);
  
  
	fa_string_t json_string = fa_string("[1, 2, 3]");
	fa_log_info(json_string);
  
    fa_log_region_count(NULL);
	
	//JSON_Value *json = json_parse_string(fa_unstring(string));
	
    fa_ptr_t data = fa_string_from_json(json_string); // misnomer, it's not a string that is created but an object
    fa_print("data  ==> %s\n", data);
    //fa_print("head  ==> %s\n", fa_list_head(data));
    
    fa_log_region_count(NULL);
	
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
    
    fa_log_region_count(NULL);
    fa_log_info(fa_string("destroying data"));
    fa_deep_destroy_always(data);
    
    fa_log_region_count(NULL);
    fa_log_info(fa_string("destroying json_string"));
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
	
	fa_log_region_count(NULL);
	fa_string_t str = fa_string("nisse");
	fa_print("ape 1 %s", str);
	fa_log_region_count(NULL);
	fa_print("ape 2 %s", str);
	fa_deep_destroy_always(str);
	fa_log_region_count(NULL);
	
    fa_log_info(fa_string("-------------------"));
    fa_log_region_count(NULL);
	//fa_log_region_count();
	//fa_dlog_info(fa_string("ape"));
	fa_midi_message_t msg1 = fa_midi_message_create_simple(5, 7, 8);
	//fa_midi_message_t msg2 = fa_midi_message_create_simple(7, 8, 9);
	//fa_midi_message_t msg3 = fa_midi_message_create_simple(7, 8, 10);
  fa_log_info(fa_string("Before creating action"));
  fa_log_region_count(NULL);
  fa_string_t action_name = fa_string("ape");
  fa_action_t action1 = fa_action_send(action_name, msg1);
  fa_destroy(action_name);
  fa_log_info(fa_string("After creating action"));
  fa_log_region_count(NULL);
	// fa_destroy(msg1);
	// fa_destroy(msg2);
	// fa_destroy(msg3);
	//fa_log_region_count();
	//fa_dlog_info(fa_string_dshow(msg1));
	//fa_log_region_count();
	//fa_destroy(msg1);
	//fa_log_region_count();
  fa_log_info(fa_string("Before destroying action"));
  fa_log_region_count(NULL);
  fa_deep_destroy_always(action1);
  fa_log_info(fa_string("After destroying action"));
  fa_log_region_count(NULL);
	//fa_deep_destroy_always(msg2);
	//fa_deep_destroy_always(msg3);
  //fa_dlog_info(fa_string("After destroying messages"));
	//fa_log_region_count();
}
