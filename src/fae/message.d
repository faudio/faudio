

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
void doremir_message_destroy_dispatcher(doremir_message_dispatcher_t dispatcher) {}

/** Send a message to the given address using the given sender.
    @param receiver     Receiver of the message.
    @param address      Address of the message.
    @param message      Message to send.
 */
void doremir_message_send(doremir_message_receiver_t    receiver,
                          doremir_message_address_t     address,
                          doremir_message_t             message) {}

/** Syncronize the given sender.
    @param sender       Sender of the message.
 */
void doremir_message_sync(doremir_message_sender_t sender) {}

/** Query the given sender for incoming messages in the given address.
    The return value of this method will not change until sync is called again.
    @param sender       Sender of the message.
    @param  address     Address of the message.
    @return             A possibly empty list of messages.
 */
doremir_list_t doremir_message_receive(doremir_message_sender_t     sender,
                                       doremir_message_address_t    address) {}
