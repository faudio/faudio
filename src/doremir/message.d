

/** Create a dispatcher for single-threaded use.
    @return
 */
doremir_message_dispatcher_t doremir_message_create_dispatcher() {}

/** Create a dispatcher for multi-threaded use.
    @return
 */
doremir_message_dispatcher_t doremir_message_create_lockfree_dispatcher() {}

/** Destroy the given dispatcher.
    @param dispatcher
 */
void doremir_message_destroy(doremir_message_dispatcher_t dispatcher) {}

/** Send a message to the given address.
    @param address
    @param message
    @param dispatcher
 */
void doremir_message_send(doremir_message_address_t     address, 
                          doremir_message_t             message, 
                          doremir_message_dispatcher_t  dispatcher) {}

/** Query for incoming messages in the given address.
    Behaves exactly like a call to sync() followed by a call to query().
    @param
    @return
 */
doremir_list_t doremir_message_fetch(doremir_message_address_t address, 
                                     doremir_message_dispatcher_t dispatcher) {}

/** Syncronize the dispatcher, possibly acquiring incoming messages.
    @param
    @return
 */
void doremir_message_sync(doremir_message_dispatcher_t dispatcher) {}

/** Query for incoming messages in the given address.
    The return value of this method will not change until query is called again.
    @param  Address to query.
    @return A possibly empty list of messages.
 */
doremir_list_t doremir_message_query(doremir_message_address_t address, 
                                     doremir_message_dispatcher_t dispatcher) {}

