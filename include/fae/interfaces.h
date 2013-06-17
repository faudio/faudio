
#ifndef _FAE_INTERFACES
#define _FAE_INTERFACES

/*
    Ideally, Modulo would generate interface identifiers for us.
    Until that day we declare all in this enum to keep them unique.
 */
enum fae_interfaces {
  fae_copy_i,
  fae_destroy_i,
  fae_dynamic_i,
  fae_error_i,
  fae_equal_i,

  fae_time_clock_interface_i,
  fae_message_sender_interface_i,
  fae_message_receiver_interface_i,
  fae_number_i,
  fae_order_i,

  fae_processor_interface_i,
  fae_string_show_i
};

#endif // _FAE_INTERFACES

