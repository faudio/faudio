
#ifndef _DOREMIR_INTERFACES
#define _DOREMIR_INTERFACES

/*
    Ideally, Modulo would generate interface identifiers for us.
    Until that day we declare all in this enum to keep them unique.
 */
enum doremir_interfaces {
  doremir_copy_i,
  doremir_destroy_i,
  doremir_dynamic_i,
  doremir_error_i,
  doremir_equal_i,
  doremir_message_sender_i,
  doremir_message_receiver_i,
  doremir_number_i,
  doremir_order_i,
  doremir_processor_interface_i,
  doremir_string_show_i
};

#endif // _DOREMIR_INTERFACES

