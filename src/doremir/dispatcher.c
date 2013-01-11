


/**
    Creates a receiver synchronizing on any receiver.
 */
doremir_dispatcher_recv_t doremir_dispatcher_or(doremir_dispatcher_recv_t,
                                                doremir_dispatcher_recv_t)
{
}

/**
    Creates a receiver synchronizing on both receivers.
 */
doremir_dispatcher_recv_t doremir_dispatcher_and(doremir_dispatcher_recv_t,
                                                 doremir_dispatcher_recv_t)
{
}

/**
    Creates a receiver synchronizing on both receivers.
 */
doremir_dispatcher_recv_t doremir_dispatcher_first(doremir_dispatcher_recv_t,
                                                   doremir_dispatcher_recv_t)
{
}

/**
    Creates a receiver synchronizing on both receivers.
 */
doremir_dispatcher_recv_t doremir_dispatcher_sample(doremir_dispatcher_recv_t,
                                                    doremir_dispatcher_recv_t)
{
}

/**
    Creates a receiver forwarding incoming messages whose address match the given predicate.
 */
doremir_dispatcher_recv_t doremir_dispatcher_filter(doremir_pred_t,
                                                    doremir_dispatcher_recv_t)
{
}

/**
    Creates a receiver forwarding incoming pairs that match the given predicate on `(addr,msg)`.
 */
doremir_dispatcher_recv_t doremir_dispatcher_filter_pair(doremir_pred_t,
                                                         doremir_dispatcher_recv_t)
{
}

/**
    Creates a receiver/dispatchers fowarding all messages on the incoming thread.
 */
doremir_ptr_t doremir_dispatcher_simple()
{
}

/**
    Creates a pair of coupled receiver/dispatchers, forwarding incoming messages when dispatch is called.
 */
doremir_pair_t doremir_dispatcher_buffered()
{
}

/**
    Creates a pair of coupled receiver/dispatchers, forwarding incoming messages when dispatch is called.
 */
doremir_pair_t doremir_dispatcher_non_blocking()
{
}

