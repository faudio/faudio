
#include <fa/fa.h>
#include <fa/util.h>

#include <CoreMIDI/CoreMIDI.h>

/*
    This program does ...

 */                    
 
char * MYCFStringCopyUTF8String(CFStringRef aString);

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
        
            char* str = MYCFStringCopyUTF8String(name);
            printf("  Name: %s \n", str);
            
            // FIXME
            // printf("  Name: %s\n", unstring(fa_string_from_native((void*) name)));
        }
        
        long isOffline;
        status = noErr;
        status = MIDIObjectGetIntegerProperty(device, kMIDIPropertyOffline, &isOffline);
        if (/*status != noErr*/1) {
            printf("  Offline: %ld\n", isOffline);
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
    printf("Devices: %d Sources: %d Destinations: %d \n", numDevs, numSources, numDests);
}



void midi_listener(const MIDINotification *message, void *data)
{
    MIDINotificationMessageID id = message->messageID;

    // UInt32                    sz = message->messageSize;
    if (id == kMIDIMsgSetupChanged) {
        inform(string("Setup changed!\n"));
        print_num_devices();
        // closure_t closure = data;
        // closure->function(closure->data);
    }
}


void threaded_assert(int status, char* msg)
{
    if (!status) {
        fa_fa_log_error(string(msg));
    }
}


ptr_t run_external_loop(ptr_t _)
{
    fa_print_ln(string("Running the external loop"));
    fa_print_ln(fa_string_show(fa_thread_current()));

    OSStatus status;
    CFStringRef name = fa_string_to_native(string("fa_test_core_midi"));

    MIDIClientRef client;
    status = MIDIClientCreate(name, midi_listener, NULL, &client);
    threaded_assert ((status == noErr), "MIDIClientCreate failed");

    CFRunLoopRun();
    // mark_used(client);
    assert(false && "Unreachable");
}


ptr_t run_internal_loop(ptr_t _)
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
        print_devices_with_status();

        status = MIDIClientDispose(client);
        assert ((status == noErr) && "MIDIClientDispose failed");

        fa_thread_sleep(500);
        mark_used(status);
    }
    assert(false && "Unreachable");
}

int main(int argc, char const *argv[])
{            
    fa_fa_set_log_std();
    fa_fa_initialize();
    fa_print_ln(fa_string_show(fa_thread_current()));    

    // run_internal_loop(NULL);
    // run_external_loop(NULL);


    fa_thread_create(run_external_loop, NULL);
    fa_thread_sleep(100000);

    fa_fa_terminate();
}








// TODO


char * MYCFStringCopyUTF8String(CFStringRef aString) {
  if (aString == NULL) {
    return NULL;
  }

  CFIndex length = CFStringGetLength(aString);
  CFIndex maxSize =
  CFStringGetMaximumSizeForEncoding(length,
                                    kCFStringEncodingUTF8);
  char *buffer = (char *)malloc(maxSize);
  if (CFStringGetCString(aString, buffer, maxSize,
                         kCFStringEncodingUTF8)) {
    return buffer;
  }
  return NULL;
}