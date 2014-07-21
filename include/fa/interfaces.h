
#ifndef _FA_INTERFACES
#define _FA_INTERFACES

/*
    Ideally, Modulo would know about interfaces and generate unique identifiers for us.

    Until that day we declare them all in this file.
 */
enum fa_interfaces {
    fa_copy_i,
    fa_destroy_i,
    fa_dynamic_i,
    fa_error_i,
    fa_equal_i,
    fa_order_i,
    fa_number_i,
    fa_clock_interface_i,
    fa_io_filter_interface_i,
    fa_string_show_i
};

#endif // _FA_INTERFACES

