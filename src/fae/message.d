

/** Create a dispatcher for single-threaded use.
    @return
 */
fae_message_dispatcher_t fae_message_create_dispatcher() {}

/** Create a dispatcher for multi-threaded use.
    @return
 */
fae_message_dispatcher_t fae_message_create_lockfree_dispatcher() {}

/** Destroy the given dispatcher.
    @param dispatcher
 */
void fae_message_destroy_dispatcher(fae_message_dispatcher_t dispatcher) {}

/** Send a message to the given address using the given sender.
    @param receiver     Receiver of the message.
    @param address      Address of the message.
    @param message      Message to send.
 */
void fae_message_send(fae_message_receiver_t    receiver,
                          fae_message_address_t     address,
                          fae_message_t             message) {}

/** Syncronize the given sender.
    @param sender       Sender of the message.
 */
void fae_message_sync(fae_message_sender_t sender) {}

/** Query the given sender for incoming messages in the given address.
    The return value of this method will not change until sync is called again.
    @param sender       Sender of the message.
    @param  address     Address of the message.
    @return             A possibly empty list of messages.
 */
fae_list_t fae_message_receive(fae_message_sender_t     sender,
                                       fae_message_address_t    address) {}
