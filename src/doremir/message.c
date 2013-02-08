
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/message.h>
#include <doremir/atomic/queue.h>
#include <doremir/map.h>
#include <doremir/util.h>

/**
    Notes:
        * TODO pluggable allocation here?
            * For sender:
                doremir_pair_create
            * For receiver:
                doremir_pair_destroy
                doremir_list_empty
                doremir_list_dcons
                doremir_list_destroy
                doremir_map_empty
                doremir_map_dset
                doremir_map_destroy

 */

#define max_recv_k  1000

struct _doremir_message_dispatcher_t {
    impl_t                      impl;               //  Interface dispatcher

    bool                        is_lockfree;
    union {
        struct {

        }                       simple;
        struct {
            atomic_queue_t      queue;      // Queue (Addr,Msg)
            map_t               mailbox;    // Map Addres [Msg]

        }                       lockfree;
    }                           fields;
};

#define lockfree_get(v,f)   v->fields.lockfree.f


// --------------------------------------------------------------------------------

void message_fatal(char *msg, int error);

dispatcher_t new_dispatcher()
{
    ptr_t dispatcher_impl(doremir_id_t interface);

    dispatcher_t p = doremir_new(message_dispatcher);
    p->impl = &dispatcher_impl;

    return p;
}

void delete_dispatcher(dispatcher_t p)
{
    doremir_delete(p);
}

// --------------------------------------------------------------------------------

dispatcher_t doremir_message_create_dispatcher()
{
    // TODO use lockfree for now
    return doremir_message_create_lockfree_dispatcher();
}

dispatcher_t doremir_message_create_lockfree_dispatcher()
{
    dispatcher_t dispatcher = new_dispatcher();

    dispatcher->is_lockfree           = true;
    lockfree_get(dispatcher, queue)   = atomic_queue();
    lockfree_get(dispatcher, mailbox) = doremir_map_empty();

    return dispatcher;
}

void doremir_message_destroy_dispatcher(dispatcher_t dispatcher)
{
    if (!dispatcher->is_lockfree) {
        assert(false && "Unreachable");
    } else {
        doremir_destroy(lockfree_get(dispatcher, queue));
        doremir_destroy(lockfree_get(dispatcher, mailbox));
    }

    delete_dispatcher(dispatcher);
}

void doremir_message_dispatcher_send(dispatcher_t dispatcher,
                                     address_t address,
                                     doremir_message_t message)
{
    if (!dispatcher->is_lockfree) {
        assert(false && "Unreachable");
    } else {
        pair_t bucket = pair(address, message);

        if (!doremir_atomic_queue_write(lockfree_get(dispatcher, queue), bucket)) {
            message_fatal("Could not write to queue", -1);
        }
    }
}

inline static ptr_t with_default(ptr_t def, ptr_t value)
{
    return value ? value : def;
}
inline static ptr_t with_dest_default(ptr_t def, ptr_t value)
{
    if (value) {
        doremir_destroy(def);
        return value;
    } else {
        return def;
    }
}

void doremir_message_dispatcher_sync(dispatcher_t dispatcher)
{
    if (!dispatcher->is_lockfree) {
        assert(false && "Unreachable");
    } else {

        // Clear mailbox
        doremir_destroy(lockfree_get(dispatcher, mailbox)); // TODO should be deep destroy
        lockfree_get(dispatcher, mailbox) = doremir_map_empty();

        // Fetch up to max_recv_k buckets into mailbox
        for (int i = 0; i < max_recv_k; ++i) {
            pair_t bucket = doremir_atomic_queue_read(lockfree_get(dispatcher, queue));

            if (!bucket) {
                break;

            } else {
                address_t address = doremir_pair_fst(bucket);
                message_t message = doremir_pair_snd(bucket);

                // TODO reclaim bucket

                list_t current = with_dest_default(
                                     doremir_list_empty(),
                                     doremir_map_get(address,
                                                     lockfree_get(dispatcher, mailbox))
                                 );
                current = doremir_list_dcons(message, current);
                lockfree_get(dispatcher, mailbox) =
                    doremir_map_dset(address, current, lockfree_get(dispatcher, mailbox));
            }
        }
    }
}

list_t doremir_message_dispatcher_receive(dispatcher_t dispatcher,
                                          address_t address)
{
    if (!dispatcher->is_lockfree) {
        assert(false && "Unreachable");
    } else {
        list_t current = with_dest_default(
                             doremir_list_empty(),
                             doremir_map_get(address,
                                             lockfree_get(dispatcher, mailbox))
                         );
        return current;
    }
}


// Generic versions

void doremir_message_send(ptr_t         receiver,
                          address_t     address,
                          message_t     message)
{
    assert(doremir_interface(doremir_message_receiver_i, receiver) 
        && "Must implement Receiver");

    ((doremir_message_receiver_t*)
        doremir_interface(doremir_message_receiver_i, receiver))
            ->send(receiver, address, message);
}
void doremir_message_sync(ptr_t sender)
{
    assert(doremir_interface(doremir_message_sender_i, sender) 
        && "Must implement Sender");

    ((doremir_message_sender_t*)
        doremir_interface(doremir_message_sender_i, sender))
            ->sync(sender);
}
doremir_list_t doremir_message_receive(ptr_t        sender,
                                       address_t    address)
{
    assert(doremir_interface(doremir_message_sender_i, sender) 
        && "Must implement Sender");

    return ((doremir_message_sender_t*)
        doremir_interface(doremir_message_sender_i, sender))->receive(sender, address);
}



// --------------------------------------------------------------------------------

void dispatcher_send(ptr_t a, address_t addr, message_t msg)
{
    dispatcher_t dispatcher = (dispatcher_t) a;
    doremir_message_dispatcher_send(dispatcher, addr, msg);
}

void dispatcher_sync(ptr_t a)
{
    dispatcher_t dispatcher = (dispatcher_t) a;
    doremir_message_dispatcher_sync(dispatcher);
}

doremir_list_t dispatcher_receive(ptr_t a, address_t addr)
{
    dispatcher_t dispatcher = (dispatcher_t) a;
    return doremir_message_dispatcher_receive(dispatcher, addr);
}

doremir_string_t dispatcher_show(ptr_t a)
{
    string_t str = string("<Dispatcher");
    str = string_dappend(str, doremir_string_format_integer(" %p", (long) a));
    str = string_dappend(str, string(">"));
    return str;
}

void dispatcher_destroy(ptr_t a)
{
    doremir_message_destroy_dispatcher(a);
}

ptr_t dispatcher_impl(doremir_id_t interface)
{
    static doremir_string_show_t dispatcher_show_impl
        = { dispatcher_show };
    static doremir_destroy_t dispatcher_destroy_impl
        = { dispatcher_destroy };
    static doremir_message_receiver_t dispatcher_message_receiver_impl
        = { dispatcher_send };
    static doremir_message_sender_t dispatcher_message_sender_impl
        = { dispatcher_sync, dispatcher_receive };

    switch (interface) {
    case doremir_string_show_i:
        return &dispatcher_show_impl;

    case doremir_destroy_i:
        return &dispatcher_destroy_impl;

    case doremir_message_sender_i:
        return &dispatcher_message_sender_impl;

    case doremir_message_receiver_i:
        return &dispatcher_message_receiver_impl;

    default:
        return NULL;
    }
}


void doremir_audio_engine_log_error_from(doremir_string_t msg, doremir_string_t origin);

void message_fatal(char *msg, int error)
{
    doremir_audio_engine_log_error_from(string_dappend(string(msg), format_integer(" (error code %d)", error)), string("Doremir.message"));
    doremir_audio_engine_log_error(string("Terminating Audio Engine"));
    exit(error);
}
