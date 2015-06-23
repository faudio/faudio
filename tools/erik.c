
#include <fa/fa.h>
#include <fa/util.h>
#include <fa/dynamic.h>
#include "common.h"

#include <stdio.h>
#include <sys/types.h> 
#include <sys/socket.h>
#include <netinet/in.h>
#include <unistd.h>

/*
Erik's test file
*/


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

	// for (int x = 0; x < 3; ++x) {
// 		for (int i = 0; i < 10; ++i) {
//
// 			// fa_clock_t cl = fa_clock_standard();
// 			// fa_clock_t cl = fa_audio_stream_clock(st);
// 			// fa_mark_used(cl);
//
// 			// printf("Scheduling msec: %lld \n", fa_clock_milliseconds(cl));
// 			// printf("Scheduling time: %s \n", unstring(fa_string_show(fa_clock_time(cl))));
//
// 			fa_action_t chord = fa_action_many(list(
// 				fa_pair_create(
// 					fa_action_send(name, fa_midi_message_create_simple(0x90, 64 + ((i % 12) * 3), 90)),
// 			fa_hms(0, 0, 0)
// 				),
// 			fa_pair_create(
// 				fa_action_send(name, fa_midi_message_create_simple(0x90, 60 + ((i % 12) * 3), 90)),
// 			fa_hms(0, 0, 0)
// 				)
// 					));
// 			// printf("System time (early): %lld\n", fa_clock_milliseconds(fa_clock_standard()));
// 			fa_audio_schedule_relative(
// 				fa_hms(0, 0, 0),
// 			chord,
// 			st);
// 			fa_log_region_count();
// 			fa_thread_sleep(150);
// 			fa_log_region_count();
// 		}
// 		fa_log_region_count();
// 	}
	// fa_log_region_count();
	// fa_dlog_info(fa_string("Now sleeping..."));
	// fa_thread_sleep(2000);
	// fa_log_region_count();
	
	fa_dlog_info(fa_string("Listening..."));
	
	{
		int sockfd, newsockfd, portno; //, clilen;
		unsigned int clilen;
		char buffer[256];
		struct sockaddr_in serv_addr, cli_addr;
		int n;
		sockfd = socket(AF_INET, SOCK_STREAM, 0);
		if (sockfd < 0) {
			fa_log_error(fa_string("ERROR opening socket"));
			exit(1);
		}
		bzero((char *) &serv_addr, sizeof(serv_addr));
		portno = 41567;
		serv_addr.sin_family = AF_INET;
		serv_addr.sin_addr.s_addr = INADDR_ANY;
		serv_addr.sin_port = htons(portno);
		if (bind(sockfd, (struct sockaddr *) &serv_addr,
		sizeof(serv_addr)) < 0) {
			fa_log_error(fa_string("ERROR on binding"));
			exit(1);
		}
		listen(sockfd,5);
		clilen = sizeof(cli_addr);
		newsockfd = accept(sockfd, (struct sockaddr *) &cli_addr, &clilen);
		if (newsockfd < 0) {
			fa_log_error(fa_string("ERROR on accept"));
			exit(1);
		}
		bzero(buffer,256);
		n = read(newsockfd,buffer,255);
		if (n < 0) {
			fa_log_error(fa_string("ERROR reading from socket"));
			exit(1);
		}
		printf("Here is the message: %s\n",buffer);
		n = write(newsockfd,"I got your message",18);
		if (n < 0) {
			fa_log_error(fa_string("ERROR writing to socket"));
			exit(1);

		}
	}
	
	fa_dlog_info(fa_string("Stopped listening"));

	fa_destroy(st);
	fa_destroy(s);
	
}

int main(int argc, char const *argv[])
{
	fa_set_log_tool();
	
	
	fa_string_t json_string = fa_string("[1, 2, 3, [4, 5], 6]");
	fa_log_info(json_string);
	
	//JSON_Value *json = json_parse_string(fa_unstring(string));
	
    fa_ptr_t data = fa_string_from_json(json_string); // misnomer, it's not a string that is created but an object
    fa_print("data  ==> %s\n", data);
	fa_print("head  ==> %s\n", fa_list_head(data));
	
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
	
	fa_log_region_count();
	fa_string_t str = fa_string("nisse");
	fa_print("ape 1 %s", str);
	fa_log_region_count();
	fa_print("ape 2 %s", str);
	fa_destroy(str);
	fa_log_region_count();
	
	//fa_log_region_count();
	//fa_dlog_info(fa_string("ape"));
	fa_midi_message_t msg1 = fa_midi_message_create_simple(5, 7, 8);
	fa_midi_message_t msg2 = fa_midi_message_create_simple(7, 8, 9);
	fa_midi_message_t msg3 = fa_midi_message_create_simple(7, 8, 10);
	// fa_destroy(msg1);
	// fa_destroy(msg2);
	// fa_destroy(msg3);
	//fa_log_region_count();
	fa_dlog_info(fa_string_dshow(msg1));
	//fa_log_region_count();
	//fa_destroy(msg1);
	//fa_log_region_count();
	fa_destroy(msg2);
	fa_destroy(msg3);
	fa_log_region_count();
}
