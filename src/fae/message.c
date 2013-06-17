
/*
    FAE
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <fae/message.h>
#include <fae/atomic/queue.h>
#include <fae/map.h>
#include <fae/util.h>

/**
    Notes:
        * TODO pluggable allocation here?
            * For sender:
                fae_pair_create
            * For receiver:
                fae_pair_destroy
                fae_list_empty
                fae_list_dcons
                fae_list_destroy
                fae_map_empty
                fae_map_dset
                fae_map_destroy

 */

#define max_recv_k  1000

struct _fae_message_dispatcher_t {
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
    ptr_t dispatcher_impl(fae_id_t interface);

    dispatcher_t p = fae_new(message_dispatcher);
    p->impl = &dispatcher_impl;

    return p;
}

void delete_dispatcher(dispatcher_t p)
{
    fae_delete(p);
}

// --------------------------------------------------------------------------------

dispatcher_t fae_message_create_dispatcher()
{
    // TODO use lockfree for now
    return fae_message_create_lockfree_dispatcher();
}

dispatcher_t fae_message_create_lockfree_dispatcher()
{
    dispatcher_t dispatcher = new_dispatcher();

    dispatcher->is_lockfree           = true;
    lockfree_get(dispatcher, queue)   = atomic_queue();
    lockfree_get(dispatcher, mailbox) = fae_map_empty();

    return dispatcher;
}

void fae_message_destroy_dispatcher(dispatcher_t dispatcher)
{
    if (!dispatcher->is_lockfree) {
        assert(false && "Unreachable");
    } else {
        fae_destroy(lockfree_get(dispatcher, queue));
        fae_destroy(lockfree_get(dispatcher, mailbox));
    }

    delete_dispatcher(dispatcher);
}

void fae_message_dispatcher_send(dispatcher_t dispatcher,
                                     address_t address,
                                     fae_message_t message)
{
    if (!dispatcher->is_lockfree) {
        assert(false && "Unreachable");
    } else {
        pair_t bucket = pair(address, message);

        if (!fae_atomic_queue_write(lockfree_get(dispatcher, queue), bucket)) {
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
        fae_destroy(def);
        return value;
    } else {
        return def;
    }
}

void fae_message_dispatcher_sync(dispatcher_t dispatcher)
{
    if (!dispatcher->is_lockfree) {
        assert(false && "Unreachable");
    } else {

        // Clear mailbox
        fae_destroy(lockfree_get(dispatcher, mailbox)); // TODO should be deep destroy
        lockfree_get(dispatcher, mailbox) = fae_map_empty();

        // Fetch up to max_recv_k buckets into mailbox
        for (int i = 0; i < max_recv_k; ++i) {
            pair_t bucket = fae_atomic_queue_read(lockfree_get(dispatcher, queue));

            if (!bucket) {
                break;

            } else {
                address_t address = fae_pair_fst(bucket);
                message_t message = fae_pair_snd(bucket);

                // TODO reclaim bucket

                list_t current = with_dest_default(
                                     fae_list_empty(),
                                     fae_map_get(address,
                                                     lockfree_get(dispatcher, mailbox))
                                 );
                current = fae_list_dcons(message, current);
                lockfree_get(dispatcher, mailbox) =
                    fae_map_dset(address, current, lockfree_get(dispatcher, mailbox));
            }
        }
    }
}

list_t fae_message_dispatcher_receive(dispatcher_t dispatcher,
                                          address_t address)
{
    if (!dispatcher->is_lockfree) {
        assert(false && "Unreachable");
    } else {
        list_t current = with_dest_default(
                             fae_list_empty(),
                             fae_map_get(address,
                                             lockfree_get(dispatcher, mailbox))
                         );
        return fae_list_reverse(current);
    }
}


// Generic versions

void fae_message_send(fae_message_receiver_t    receiver,
                          address_t                     address,
                          message_t                     message)
{
    assert(fae_interface(fae_message_receiver_interface_i, receiver)
           && "Must implement Receiver");

    ((fae_message_receiver_interface_t *)
     fae_interface(fae_message_receiver_interface_i, receiver))
    ->send(receiver, address, message);
}

void fae_message_sync(fae_message_sender_t sender)
{
    assert(fae_interface(fae_message_sender_interface_i, sender)
           && "Must implement Sender");

    ((fae_message_sender_interface_t *)
     fae_interface(fae_message_sender_interface_i, sender))
    ->sync(sender);
}

fae_list_t fae_message_receive(fae_message_sender_t sender,
                                       address_t                address)
{
    assert(fae_interface(fae_message_sender_interface_i, sender)
           && "Must implement Sender");

    return ((fae_message_sender_interface_t *)
            fae_interface(fae_message_sender_interface_i, sender))->receive(sender, address);
}



// --------------------------------------------------------------------------------

void dispatcher_send(ptr_t a, address_t addr, message_t msg)
{
    dispatcher_t dispatcher = (dispatcher_t) a;
    fae_message_dispatcher_send(dispatcher, addr, msg);
}

void dispatcher_sync(ptr_t a)
{
    dispatcher_t dispatcher = (dispatcher_t) a;
    fae_message_dispatcher_sync(dispatcher);
}

fae_list_t dispatcher_receive(ptr_t a, address_t addr)
{
    dispatcher_t dispatcher = (dispatcher_t) a;
    return fae_message_dispatcher_receive(dispatcher, addr);
}

fae_string_t dispatcher_show(ptr_t a)
{
    string_t str = string("<Dispatcher");
    str = string_dappend(str, fae_string_format_integral(" %p", (long) a));
    str = string_dappend(str, string(">"));
    return str;
}

void dispatcher_destroy(ptr_t a)
{
    fae_message_destroy_dispatcher(a);
}

ptr_t dispatcher_impl(fae_id_t interface)
{
    static fae_string_show_t dispatcher_show_impl
        = { dispatcher_show };
    static fae_destroy_t dispatcher_destroy_impl
        = { dispatcher_destroy };
    static fae_message_receiver_interface_t dispatcher_message_receiver_interface_impl
        = { dispatcher_send };
    static fae_message_sender_interface_t dispatcher_message_sender_interface_impl
        = { dispatcher_sync, dispatcher_receive };

    switch (interface) {
    case fae_string_show_i:
        return &dispatcher_show_impl;

    case fae_destroy_i:
        return &dispatcher_destroy_impl;

    case fae_message_sender_interface_i:
        return &dispatcher_message_sender_interface_impl;

    case fae_message_receiver_interface_i:
        return &dispatcher_message_receiver_interface_impl;

    default:
        return NULL;
    }
}


void fae_audio_engine_log_error_from(fae_string_t msg, fae_string_t origin);

void message_fatal(char *msg, int error)
{
    fae_audio_engine_log_error_from(string_dappend(string(msg), format_integral(" (error code %d)", error)), string("Doremir.message"));
    fae_audio_engine_log_error(string("Terminating Audio Engine"));
    exit(error);
}
