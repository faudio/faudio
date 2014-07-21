
#include <fa/fa.h>
#include <fa/util.h>
#include "common.h"

#include <CoreMIDI/CoreMIDI.h>

/*
    This program does ...

 */

char *get_cfstring(CFStringRef aString);

void print_devices_with_status()
{

    OSStatus status;
    int numDevs     = MIDIGetNumberOfDevices();

    for (int i = 0; i < numDevs; ++i) {
        MIDIDeviceRef device = MIDIGetDevice(i);

        CFStringRef name;
        status = noErr;
        status = MIDIObjectGetStringProperty(device, kMIDIPropertyName, &name);

        if (/*status != noErr*/1) {

            char *str = get_cfstring(name);
            printf("  Name: %s \n", str);

            // FIXME
            // printf("  Name: %s\n", unstring(fa_string_from_native((void*) name)));
        }

        SInt32 isOffline;
        status = noErr;
        status = MIDIObjectGetIntegerProperty(device, kMIDIPropertyOffline, &isOffline);

        if (/*status != noErr*/1) {
            if (isOffline) {
                printf("  -\n");
            } else {
                printf("  Online\n");
            }
        }

        mark_used(device);
        mark_used(status);
    }
}

void print_num_devices()
{
    int numDevs     = MIDIGetNumberOfDevices();
    int numSources  = MIDIGetNumberOfSources();
    int numDests    = MIDIGetNumberOfDestinations();
    printf("Devices: %d,  Sources: %d,  Destinations: %d \n", numDevs, numSources, numDests);
}


fa_midi_session_t print_midi_devices_using_fa(fa_ptr_t _, midi_session_t session)
{
    // fa_print("Listing MIDI devices:\n", 0);
    // fa_print_ln(fa_string(""));

    fa_for_each(x, fa_midi_all(session)) {
        fa_print("Name: %s\n", fa_string_to_string(fa_midi_name(x)));
        fa_print("Host: %s\n", fa_string_to_string(fa_midi_host_name(x)));
        fa_print("In:   %s\n", fb(fa_midi_has_input(x)));
        fa_print("Out:  %s\n", fb(fa_midi_has_output(x)));
        fa_print_ln(fa_string(""));
    }
    return session;
}


void midi_listener(const MIDINotification *message, void *data)
{
    MIDINotificationMessageID id = message->messageID;

    // UInt32                    sz = message->messageSize;
    if (id == kMIDIMsgSetupChanged) {
        inform(fa_string("Setup changed!\n"));
        print_num_devices();
        print_devices_with_status();

        // fa_midi_with_session(
        //     print_midi_devices_using_fa, NULL,
        //     fa_error_log, NULL);

    }
}


void threaded_assert(int status, char *msg)
{
    if (!status) {
        fa_log_error(fa_string(msg));
    }
}


ptr_t run_external_loop(ptr_t _)
{
    fa_print_ln(fa_string("Running the external loop"));
    fa_print_ln(fa_string_show(fa_thread_current()));

    OSStatus status;
    CFStringRef name = fa_string_to_native(fa_string("fa_test_core_midi"));

    MIDIClientRef client;
    status = MIDIClientCreate(name, midi_listener, NULL, &client);
    threaded_assert((status == noErr), "MIDIClientCreate failed");

    CFRunLoopRun();
    // mark_used(client);
    assert(false && "Unreachable");
}


ptr_t run_internal_loop(ptr_t _)
{
    fa_print_ln(fa_string("Running the internal loop"));

    OSStatus status;
    CFStringRef name = fa_string_to_native(fa_string("fa_test_core_midi"));

    while (1) {
        MIDIClientRef client;

        status = MIDIRestart();
        assert((status == noErr) && "MIDIRestart failed");

        status = MIDIClientCreate(name, NULL, NULL, &client);
        assert((status == noErr) && "MIDIClientCreate failed");

        print_num_devices();
        print_devices_with_status();

        status = MIDIClientDispose(client);
        assert((status == noErr) && "MIDIClientDispose failed");

        fa_thread_sleep(500);
        mark_used(status);
    }

    assert(false && "Unreachable");
}

int main(int argc, char const *argv[])
{
    fa_set_log_tool();
    fa_initialize();
    fa_print_ln(fa_string_show(fa_thread_current()));

    // run_internal_loop(NULL);
    // run_external_loop(NULL);


    fa_thread_create(run_external_loop, NULL);

    fa_thread_sleep(1000);
    MIDIGetNumberOfDestinations();
    MIDIGetNumberOfSources();
    fa_thread_sleep(1000);

    fa_terminate();
}








// TODO

char *get_cfstring(CFStringRef aString)
{
    string_t s = fa_string_from_native((void *) aString);
    return unstring(s);
}
