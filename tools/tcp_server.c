
#include <fa/fa.h>
#include <fa/util.h>
#include <fa/dynamic.h>
#include <fa/string.h>
#include "common.h"

#include <stdio.h>
#include <sys/types.h> 
#include <sys/socket.h>
#include <netinet/in.h>
#include <unistd.h>

#include <lo/lo.h>

/*
Sound server test
*/

const int MSG_MIDI = 1;
const int MSG_TEST = 2;

int readXBytes(int socket, unsigned int x, void* buffer)
{
	int bytesRead = 0;
	int result;
	while (bytesRead < x)
	{
		result = read(socket, buffer + bytesRead, x - bytesRead);
		if (result < 1)
		{
			return result;
		}
		bytesRead += result;
	}
	return bytesRead;
}

int processMessage(fa_ptr_t message)
{
	if ((fa_dynamic_get_type(message) == list_type_repr) &&
		(!fa_list_is_empty(message)) &&
		(fa_dynamic_get_type(fa_list_head(message)) == i32_type_repr))
	{
	    switch (fa_peek_int32(fa_list_head(message))) {
	    case 1:
			printf("a MIDI message!\n");
			break;

	    case 2:
			printf("it's a set!\n");
			break;
		}
		
	}
	return 0;
}

int main(int argc, char const *argv[])
{
	
	int sockfd, newsockfd, portno;
	unsigned int clilen;
	struct sockaddr_in serv_addr, cli_addr;
	int n;
	int run = 1;
		
	fa_set_log_tool();
	fa_log_info(fa_string("Listening..."));
		
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
		
		
	while (run) {
		newsockfd = accept(sockfd, (struct sockaddr *) &cli_addr, &clilen);
		if (newsockfd < 0) {
			fa_log_error(fa_string("ERROR on accept"));
			run = 0;
			break;
		}
			
		while (run) {

			unsigned int msglen = 0;
			char* buffer = 0;
			if (readXBytes(newsockfd, sizeof(msglen), (void*)(&msglen)) <= 0)
			{
				fa_log_info(fa_string("Disconnected"));
				run = 0;
				break;
			}


			printf("New message: %d bytes\n", msglen);
			buffer = fa_malloc(msglen+1);
			readXBytes(newsockfd, msglen, (void*)buffer);
			buffer[msglen] = 0;
		
			printf("Here is the message: %s\n",buffer);
				
			{
				fa_string_t str = fa_string_from_utf8(buffer);
				fa_ptr_t msg = fa_string_from_json(str);
					
				fa_log_info(fa_string_show(msg));
				
				processMessage(msg);
				
				fa_destroy(msg);
				fa_destroy(str);

			}				
			fa_mark_used(n);
			// n = write(newsockfd,"I got your message",18);
			// 				if (n < 0) {
			// 					fa_log_error(fa_string("ERROR writing to socket"));
			// 					exit(1);
			//
			// 				}
			fa_free(buffer);
		}
	}
	
	fa_log_info(fa_string("Stopped listening"));	
}

