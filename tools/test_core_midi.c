
#include <fa/fa.h>
#include <fa/util.h>

#include <CoreMIDI/CoreMIDI.h>

/*
    This program does ...

 */
void print_num_devices()
{
    int numDevs     = MIDIGetNumberOfDevices();
    int numSources  = MIDIGetNumberOfSources();
    int numDests    = MIDIGetNumberOfDestinations();        
    printf("Devices: %d Sources: %d Destinations: %d \n", numDevs, numSources, numDests);
}



void midi_listener(const MIDINotification *message, void *data)
{
    MIDINotificationMessageID id = message->messageID;

    // UInt32                    sz = message->messageSize;
    if (id == kMIDIMsgSetupChanged) {
        printf("Setup changed!\n");
        print_num_devices();
        // closure_t closure = data;
        // closure->function(closure->data);
    }
}


void run_external_loop()
{
    fa_print_ln(string("Running the external loop"));

    OSStatus status;
    CFStringRef name = fa_string_to_native(string("fa_test_core_midi"));

    MIDIClientRef client;
    status = MIDIClientCreate(name, midi_listener, NULL, &client);
    assert ((status == noErr) && "MIDIClientCreate failed");

    CFRunLoopRun();
    mark_used(client);
}


void run_internal_loop()
{
    fa_print_ln(string("Running the internal loop"));

    OSStatus status;
    CFStringRef name = fa_string_to_native(string("fa_test_core_midi"));

    while(1) {               
        MIDIClientRef client;                                 
        
        status = MIDIRestart();
        assert ((status == noErr) && "MIDIRestart failed");

        status = MIDIClientCreate(name, NULL, NULL, &client);
        assert ((status == noErr) && "MIDIClientCreate failed");

        print_num_devices();

        status = MIDIClientDispose(client);
        assert ((status == noErr) && "MIDIClientDispose failed");

        fa_thread_sleep(500);
        mark_used(status);
    }
}

int main(int argc, char const *argv[])
{
    fa_fa_initialize();

    // run_internal_loop();
    run_external_loop();

    fa_fa_terminate();
}
